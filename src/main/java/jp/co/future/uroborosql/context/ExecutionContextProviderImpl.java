/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.context;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.JarURLConnection;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.ResultSet;
import java.time.Clock;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.StringJoiner;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * ExecutionContextプロバイダ実装
 *
 * @author H.Sugimoto
 */
public class ExecutionContextProviderImpl implements ExecutionContextProvider {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(ExecutionContextProviderImpl.class);

	/** 定数パラメータプレフィックス */
	private String constParamPrefix = "CLS_";

	/** 定数クラス名（FQDN） */
	private List<String> constantClassNames = new ArrayList<>();
	/** Enum定数パッケージ名 */
	private List<String> enumConstantPackageNames = new ArrayList<>();

	/** 定数パラメータマップ */
	private Map<String, Parameter> constParameterMap = null;

	/** SQL設定クラス */
	private SqlConfig sqlConfig = null;

	/** 自動パラメータバインド関数List(query用) */
	private final List<Consumer<ExecutionContext>> queryAutoParameterBinders = new ArrayList<>();

	/** 自動パラメータバインド関数List(update/batch/proc用) */
	private final List<Consumer<ExecutionContext>> updateAutoParameterBinders = new ArrayList<>();

	/** 合成自動パラメータバインド関数(query用) */
	private Consumer<ExecutionContext> queryAutoParameterBinder = null;

	/** 合成自動パラメータバインド関数(update/batch/proc用) */
	private Consumer<ExecutionContext> updateAutoParameterBinder = null;

	/** ResultSetTypeの初期値 */
	private int defaultResultSetType = ResultSet.TYPE_FORWARD_ONLY;

	/** ResultSetConcurrencyの初期値*/
	private int defaultResultSetConcurrency = ResultSet.CONCUR_READ_ONLY;

	/** パラメータ変換マネージャ */
	private BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager(
			Clock.systemDefaultZone());

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.ExecutionContextProvider#createExecutionContext()
	 */
	@Override
	public ExecutionContext createExecutionContext() {
		var executionContext = new ExecutionContextImpl();
		Map<String, Parameter> paramMap = new ConcurrentHashMap<>(getConstParameterMap());

		executionContext.setConstParameterMap(paramMap);
		executionContext.setSqlFilterManager(getSqlConfig().getSqlFilterManager());
		executionContext.setParameterMapperManager(
				new BindParameterMapperManager(parameterMapperManager, getSqlConfig().getClock()));
		executionContext.setQueryAutoParameterBinder(queryAutoParameterBinder);
		executionContext.setUpdateAutoParameterBinder(updateAutoParameterBinder);
		executionContext.setResultSetType(defaultResultSetType);
		executionContext.setResultSetConcurrency(defaultResultSetConcurrency);

		return executionContext;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfigAware#setSqlConfig(jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public void setSqlConfig(final SqlConfig sqlConfig) {
		this.sqlConfig = sqlConfig;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfigAware#getSqlConfig()
	 */
	@Override
	public SqlConfig getSqlConfig() {
		return sqlConfig;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.ExecutionContextProvider#initialize()
	 */
	@Override
	public void initialize() {
		parameterMapperManager = new BindParameterMapperManager(parameterMapperManager, getSqlConfig().getClock());

		Map<String, Parameter> paramMap = new HashMap<>(buildConstParamMap());
		paramMap.putAll(buildEnumConstParamMap());
		constParameterMap = Collections.unmodifiableMap(paramMap);
	}

	/**
	 * 定数パラメータのMapを生成する
	 *
	 * @param paramMap 定数パラメータを保持するMap
	 * @param targetClass 定数パラメータを生成する定数クラス。クラス内に内部クラスを持つ場合は内部クラスの定数フィールドもパラメータに登録する
	 */
	protected void makeConstParamMap(final Map<String, Parameter> paramMap, final Class<?> targetClass) {
		try {
			var fieldPrefix = targetClass.isMemberClass() ? CaseFormat.UPPER_SNAKE_CASE
					.convert(targetClass.getSimpleName()) + "_" : "";
			// 指定されたクラス直下の定数フィールドを追加
			var fields = targetClass.getFields();
			for (Field field : fields) {
				var mod = field.getModifiers();
				if (Modifier.isFinal(mod) && Modifier.isStatic(mod)) {
					var value = field.get(null);
					if (parameterMapperManager.canAcceptByStandard(value)) {
						var fieldName = getConstParamPrefix() + fieldPrefix + field.getName();
						fieldName = fieldName.toUpperCase();
						var newValue = new Parameter(fieldName, field.get(null));
						var prevValue = paramMap.put(fieldName, newValue);
						if (prevValue != null) {
							LOG.warn("Duplicate constant name. Constant name:{}, Old name:{} destroy.", fieldName,
									prevValue.getValue());
						}
						LOG.debug("Constant [name:{}, value:{}] added to parameter.", fieldName, newValue.getValue());
					}
				}
			}

			// 内部クラスを持つ場合
			var memberClasses = targetClass.getDeclaredClasses();
			for (Class<?> memberClass : memberClasses) {
				var mod = memberClass.getModifiers();
				if (Modifier.isFinal(mod) && Modifier.isPublic(mod)) {
					makeConstParamMap(paramMap, memberClass);
				}
			}
		} catch (IllegalArgumentException | IllegalAccessException | SecurityException ex) {
			LOG.error(ex.getMessage(), ex);
		}
	}

	/**
	 * Enum型の定数パラメータのMapを生成する
	 *
	 * @param paramMap 定数パラメータを保持するMap
	 * @param packageName パッケージ名
	 * @param targetClass 対象Enumクラス
	 */
	protected void makeEnumConstParamMap(final Map<String, Parameter> paramMap, final String packageName,
			final Class<? extends Enum<?>> targetClass) {

		var fieldPrefix = CaseFormat.UPPER_SNAKE_CASE.convert(targetClass.getName().substring(
				packageName.length() + 1))
				+ "_";

		Enum<?>[] enumValues = targetClass.getEnumConstants();

		for (Enum<?> value : enumValues) {
			var fieldName = getConstParamPrefix() + fieldPrefix + value.name().toUpperCase();
			fieldName = fieldName.toUpperCase();
			var newValue = new Parameter(fieldName, value);
			var prevValue = paramMap.put(fieldName, newValue);
			if (prevValue != null) {
				LOG.warn("Duplicate Enum name. Enum name:{}, Old name:{} destroy.", fieldName, prevValue.getValue());
			}
			LOG.debug("Enum [name:{}, value:{}] added to parameter.", fieldName, newValue.getValue());
		}
	}

	/**
	 * {inheritDoc}
	 *
	 * @see ExecutionContextProvider#getConstParamPrefix()
	 */
	@Override
	public String getConstParamPrefix() {
		return constParamPrefix;
	}

	/**
	 * {inheritDoc}
	 *
	 * @see ExecutionContextProvider#setConstParamPrefix(String)
	 */
	@Override
	public ExecutionContextProvider setConstParamPrefix(final String constParamPrefix) {
		this.constParamPrefix = constParamPrefix;
		return this;
	}

	/**
	 * {inheritDoc}
	 *
	 * @see ExecutionContextProvider#getConstantClassNames()
	 */
	@Override
	public List<String> getConstantClassNames() {
		return constantClassNames;
	}

	/**
	 * {inheritDoc}
	 *
	 * @see ExecutionContextProvider#setConstantClassNames(List)
	 */
	@Override
	public ExecutionContextProvider setConstantClassNames(final List<String> constantClassNames) {
		this.constantClassNames = constantClassNames;
		return this;
	}

	/**
	 * {inheritDoc}
	 *
	 * @see ExecutionContextProvider#getConstParameterMap()
	 */
	@Override
	public Map<String, Parameter> getConstParameterMap() {
		return constParameterMap;
	}

	/**
	 * {inheritDoc}
	 *
	 * @see ExecutionContextProvider#getEnumConstantPackageNames()
	 */
	@Override
	public List<String> getEnumConstantPackageNames() {
		return enumConstantPackageNames;
	}

	/**
	 * {inheritDoc}
	 *
	 * @see ExecutionContextProvider#setEnumConstantPackageNames(List)
	 */
	@Override
	public ExecutionContextProvider setEnumConstantPackageNames(final List<String> enumConstantPackageNames) {
		this.enumConstantPackageNames = enumConstantPackageNames;
		return this;
	}

	/**
	 * {inheritDoc}
	 *
	 * @see ExecutionContextProvider#addBindParamMapper(BindParameterMapper)
	 */
	@Override
	public ExecutionContextProvider addBindParamMapper(final BindParameterMapper<?> parameterMapper) {
		parameterMapperManager.addMapper(parameterMapper);
		return this;
	}

	/**
	 * {inheritDoc}
	 *
	 * @see ExecutionContextProvider#removeBindParamMapper(BindParameterMapper)
	 */
	@Override
	public ExecutionContextProvider removeBindParamMapper(final BindParameterMapper<?> parameterMapper) {
		parameterMapperManager.removeMapper(parameterMapper);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.ExecutionContextProvider#addQueryAutoParameterBinder(java.util.function.Consumer)
	 */
	@Override
	public ExecutionContextProvider addQueryAutoParameterBinder(final Consumer<ExecutionContext> binder) {
		queryAutoParameterBinders.add(binder);
		queryAutoParameterBinder = queryAutoParameterBinders.stream().reduce(Consumer::andThen)
				.orElse(null);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.ExecutionContextProvider#removeQueryAutoParameterBinder(java.util.function.Consumer)
	 */
	@Override
	public ExecutionContextProvider removeQueryAutoParameterBinder(final Consumer<ExecutionContext> binder) {
		queryAutoParameterBinders.remove(binder);
		queryAutoParameterBinder = queryAutoParameterBinders.stream().reduce(Consumer::andThen)
				.orElse(null);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.ExecutionContextProvider#addUpdateAutoParameterBinder(java.util.function.Consumer)
	 */
	@Override
	public ExecutionContextProvider addUpdateAutoParameterBinder(final Consumer<ExecutionContext> binder) {
		updateAutoParameterBinders.add(binder);
		updateAutoParameterBinder = updateAutoParameterBinders.stream()
				.reduce(Consumer::andThen)
				.orElse(null);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.ExecutionContextProvider#removeUpdateAutoParameterBinder(java.util.function.Consumer)
	 */
	@Override
	public ExecutionContextProvider removeUpdateAutoParameterBinder(final Consumer<ExecutionContext> binder) {
		updateAutoParameterBinders.remove(binder);
		updateAutoParameterBinder = updateAutoParameterBinders.stream()
				.reduce(Consumer::andThen)
				.orElse(null);
		return this;
	}

	/**
	 * 定数クラスパラメータMap生成
	 *
	 * @return 定数クラスパラメータMap
	 */
	private Map<? extends String, ? extends Parameter> buildConstParamMap() {
		Map<String, Parameter> paramMap = new HashMap<>();
		for (String className : constantClassNames) {
			if (StringUtils.isNotBlank(className)) {
				try {
					Class<?> targetClass = Class.forName(className, true, Thread.currentThread()
							.getContextClassLoader());
					makeConstParamMap(paramMap, targetClass);
				} catch (ClassNotFoundException ex) {
					LOG.error(ex.getMessage(), ex);
				}
			}
		}
		return paramMap;
	}

	/**
	 * Enum定数パラメータMap生成
	 *
	 * @return Enum定数パラメータMap
	 */
	private Map<? extends String, ? extends Parameter> buildEnumConstParamMap() {
		Map<String, Parameter> paramMap = new HashMap<>();
		for (String packageName : enumConstantPackageNames) {
			if (StringUtils.isNotBlank(packageName)) {
				for (Class<? extends Enum<?>> targetClass : listupEnumClasses(packageName)) {
					makeEnumConstParamMap(paramMap, packageName, targetClass);
				}
			}
		}
		return paramMap;
	}

	/**
	 * 対象パッケージ以下のクラスを取得
	 *
	 * @param packageName ルートパッケージ名
	 * @return クラスリスト
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static Set<Class<? extends Enum<?>>> listupEnumClasses(final String packageName) {
		var resourceName = packageName.replace('.', '/');
		var classLoader = Thread.currentThread().getContextClassLoader();
		List<URL> roots;
		try {
			roots = Collections.list(classLoader.getResources(resourceName));
		} catch (IOException e) {
			LOG.error(e.getMessage(), e);
			return Collections.emptySet();
		}

		Set<Class<?>> classes = new HashSet<>();
		for (URL root : roots) {
			if ("file".equalsIgnoreCase(root.getProtocol())) {
				try {
					classes.addAll(findEnumClassesWithFile(packageName, Paths.get(root.toURI())));
				} catch (URISyntaxException e) {
					LOG.error(e.getMessage(), e);
				}
			}
			if ("jar".equalsIgnoreCase(root.getProtocol())) {
				try (var jarFile = ((JarURLConnection) root.openConnection()).getJarFile()) {
					classes.addAll(findEnumClassesWithJar(packageName, jarFile));
				} catch (IOException e) {
					LOG.error(e.getMessage(), e);
				}
			}
		}

		return (Set) classes;
	}

	/**
	 * classファイルから対象パッケージ以下のEnumクラスを取得
	 *
	 * @param packageName ルートパッケージ名
	 * @param dir 対象ディレクトリ
	 * @return クラスリスト
	 * @throws ClassNotFoundException エラー
	 * @throws IOException
	 */
	private static Set<Class<?>> findEnumClassesWithFile(final String packageName, final Path dir) {
		Set<Class<?>> classes = new HashSet<>();
		try (var stream = Files.walk(dir)) {
			stream.filter(entry -> entry.getFileName().toString().endsWith(".class")).forEach(file -> {
				var joiner = new StringJoiner(".", packageName + ".", "");
				dir.relativize(file).forEach(p -> joiner.add(p.toString()));
				var className = joiner.toString().replaceAll(".class$", "");
				loadEnum(className).ifPresent(classes::add);
			});
		} catch (IOException e) {
			LOG.error(e.getMessage(), e);
		}

		return classes;
	}

	/**
	 * jarファイルから対象パッケージ以下のEnumクラスを取得
	 *
	 * @param packageName ルートパッケージ名
	 * @param jarFile jarファイル
	 * @return クラスリスト
	 * @throws ClassNotFoundException エラー
	 * @throws IOException
	 */
	private static Collection<? extends Class<?>> findEnumClassesWithJar(final String packageName,
			final JarFile jarFile) {
		var resourceName = packageName.replace('.', '/');
		Set<Class<?>> classes = new HashSet<>();
		Collections.list(jarFile.entries()).stream().map(JarEntry::getName)
				.filter(name -> name.startsWith(resourceName)).filter(name -> name.endsWith(".class"))
				.map(name -> name.replace('/', '.').replaceAll(".class$", ""))
				.forEach(className -> loadEnum(className).ifPresent(classes::add));

		return classes;
	}

	/**
	 * Enumクラスをロード<br>
	 * 指定クラスがEnumでない場合はemptyを返す
	 *
	 * @param className クラス名
	 * @return ロードしたEnumクラス
	 */
	private static Optional<Class<?>> loadEnum(final String className) {
		try {
			Class<?> type = Class.forName(className, true, Thread.currentThread().getContextClassLoader());
			if (type.isEnum()) {
				return Optional.of(type);
			}
		} catch (ClassNotFoundException e) {
			LOG.error(e.getMessage(), e);
		}
		return Optional.empty();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.ExecutionContextProvider#setDefaultResultSetType(int)
	 */
	@Override
	public ExecutionContextProvider setDefaultResultSetType(final int resultSetType) {
		defaultResultSetType = resultSetType;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.ExecutionContextProvider#setDefaultResultSetConcurrency(int)
	 */
	@Override
	public ExecutionContextProvider setDefaultResultSetConcurrency(final int resultSetConcurrency) {
		defaultResultSetConcurrency = resultSetConcurrency;
		return this;
	}

}
