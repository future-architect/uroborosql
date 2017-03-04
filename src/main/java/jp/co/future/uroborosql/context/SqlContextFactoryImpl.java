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
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.stream.Stream;

import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;
import jp.co.future.uroborosql.parser.TransformContext;
import jp.co.future.uroborosql.utils.CaseFormat;
import ognl.OgnlRuntime;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * SQLコンテキストファクトリ実装
 *
 * @author H.Sugimoto
 */
public class SqlContextFactoryImpl implements SqlContextFactory {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(SqlContextFactoryImpl.class);

	/** 定数パラメータプレフィックス */
	private String constParamPrefix = "CLS_";

	/** 定数クラス名（FQDN） */
	private List<String> constantClassNames = new ArrayList<>();
	/** Enum定数パッケージ名 */
	private List<String> enumConstantPackageNames = new ArrayList<>();

	/** 定数パラメータマップ */
	private Map<String, Parameter> constParameterMap = null;

	/** SqlFilter管理クラス */
	private SqlFilterManager sqlFilterManager;
	/** 自動バインド用パラメータ生成クラスのリスト */
	private List<AutoBindParameterCreator> autoBindParameterCreators = null;

	/** パラメータ変換マネージャ */
	private final BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager();

	static {
		OgnlRuntime.setPropertyAccessor(TransformContext.class, new TransformContextPropertyAccessor());
		OgnlRuntime.setPropertyAccessor(Parameter.class, new ParameterPropertyAccessor());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContextFactory#createSqlContext()
	 */
	@Override
	public SqlContext createSqlContext() {
		SqlContextImpl sqlContext = new SqlContextImpl();
		Map<String, Parameter> paramMap = new ConcurrentHashMap<>(getConstParameterMap());

		// 自動バインド用パラメータ生成クラスが指定されている場合は、そこで生成されたパラメータをパラメータマップに追加する
		if (autoBindParameterCreators != null && !autoBindParameterCreators.isEmpty()) {
			for (AutoBindParameterCreator creator : getAutoBindParameterCreators()) {
				Map<String, Parameter> bindMap = creator.getBindParameterMap();
				if (bindMap != null && !bindMap.isEmpty()) {
					paramMap.putAll(bindMap);
				}
			}
		}

		sqlContext.setConstParameterMap(paramMap);
		sqlContext.setSqlFilterManager(getSqlFilterManager());
		sqlContext.setParameterMapperManager(new BindParameterMapperManager(parameterMapperManager));
		return sqlContext;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContextFactory#initialize()
	 */
	@Override
	public void initialize() {
		Map<String, Parameter> paramMap = new HashMap<>();
		paramMap.putAll(buildConstParamMap());
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
			String fieldPrefix = targetClass.isMemberClass() ? CaseFormat.SnakeCase
					.convert(targetClass.getSimpleName()) + "_" : "";
			// 指定されたクラス直下の定数フィールドを追加
			Field[] fields = targetClass.getFields();
			for (Field field : fields) {
				int mod = field.getModifiers();
				if (Modifier.isFinal(mod) && Modifier.isPublic(mod) && Modifier.isStatic(mod)) {
					if (String.class.equals(field.getType()) || int.class.equals(field.getType())) {
						String fieldName = getConstParamPrefix() + fieldPrefix + field.getName().toUpperCase();
						fieldName = fieldName.toUpperCase();
						Parameter newValue = new Parameter(fieldName, field.get(null));
						Parameter prevValue = paramMap.put(fieldName, newValue);
						if (prevValue != null) {
							LOG.warn("定数名が重複しています。定数名：{}, 古い値：{} が破棄されます。", fieldName, prevValue.getValue());
						}
						LOG.debug("定数[名前：{}, 値：{}] を定数パラメータとして追加しました。", fieldName, newValue.getValue());
					}
				}
			}

			// 内部クラスを持つ場合
			Class<?>[] memberClasses = targetClass.getDeclaredClasses();
			for (Class<?> memberClass : memberClasses) {
				int mod = memberClass.getModifiers();
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
	 * @param paramMap 定数パラメータを保持するMap
	 * @param packageName パッケージ名
	 * @param targetClass 対象Enumクラス
	 */
	protected void makeEnumConstParamMap(final Map<String, Parameter> paramMap, final String packageName,
			final Class<? extends Enum<?>> targetClass) {

		String fieldPrefix = CaseFormat.SnakeCase.convert(targetClass.getName().substring(packageName.length() + 1))
				+ "_";

		Enum<?>[] enumValues = targetClass.getEnumConstants();

		for (Enum<?> value : enumValues) {
			String fieldName = getConstParamPrefix() + fieldPrefix + value.name().toUpperCase();
			fieldName = fieldName.toUpperCase();
			Parameter newValue = new Parameter(fieldName, value);
			Parameter prevValue = paramMap.put(fieldName, newValue);
			if (prevValue != null) {
				LOG.warn("Enum定数名が重複しています。定数名：{}, 古い値：{} が破棄されます。", fieldName, prevValue.getValue());
			}
			LOG.debug("Enum定数[名前：{}, 値：{}] を定数パラメータとして追加しました。", fieldName, newValue.getValue());
		}
	}

	/**
	 * 定数パラメータプレフィックスを取得します。
	 *
	 * @return 定数パラメータプレフィックス
	 */
	@Override
	public String getConstParamPrefix() {
		return constParamPrefix;
	}

	/**
	 * 定数パラメータプレフィックスを設定します。
	 *
	 * @param constParamPrefix
	 *            定数パラメータプレフィックス
	 */
	@Override
	public void setConstParamPrefix(final String constParamPrefix) {
		this.constParamPrefix = constParamPrefix;
	}

	/**
	 * 定数クラス名（FQDN）を取得します。
	 *
	 * @return 定数クラス名（FQDN）
	 */
	@Override
	public List<String> getConstantClassNames() {
		return constantClassNames;
	}

	/**
	 * 定数クラス名（FQDN）を設定します。
	 *
	 * @param constantClassNames
	 *            定数クラス名（FQDN）
	 */
	@Override
	public void setConstantClassNames(final List<String> constantClassNames) {
		this.constantClassNames = constantClassNames;
	}

	/**
	 * 定数クラスパラメータマップを取得します。
	 *
	 * @return 定数クラスパラメータマップ
	 */
	@Override
	public Map<String, Parameter> getConstParameterMap() {
		return constParameterMap;
	}

	/**
	 * Enum定数パッケージ名を取得します。
	 *
	 * @return Enum定数パッケージ名
	 */
	@Override
	public List<String> getEnumConstantPackageNames() {
		return enumConstantPackageNames;
	}

	/**
	 * Enum定数パッケージ名（FQDN）を設定します。
	 *
	 * @param enumConstantPackageNames Enum定数パッケージ名
	 */
	@Override
	public void setEnumConstantPackageNames(final List<String> enumConstantPackageNames) {
		this.enumConstantPackageNames = enumConstantPackageNames;
	}

	/**
	 * SqlFilter管理クラスを取得します。
	 *
	 * @return SqlFilter管理クラス
	 */
	@Override
	public SqlFilterManager getSqlFilterManager() {
		return sqlFilterManager;
	}

	/**
	 * SqlFilter管理クラスを設定します。
	 *
	 * @param sqlFilterManager SQLフィルタ管理クラス
	 *            SqlFilter管理クラス
	 */
	@Override
	public void setSqlFilterManager(final SqlFilterManager sqlFilterManager) {
		this.sqlFilterManager = sqlFilterManager;
	}

	/**
	 * パラメータ変換クラス{@link BindParameterMapper}を追加
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 */
	@Override
	public void addBindParamMapper(final BindParameterMapper<?> parameterMapper) {
		parameterMapperManager.addMapper(parameterMapper);
	}

	/**
	 * パラメータ変換クラス{@link BindParameterMapper}をremove
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 */
	@Override
	public void removeBindParamMapper(final BindParameterMapper<?> parameterMapper) {
		parameterMapperManager.removeMapper(parameterMapper);
	}

	/**
	 * 自動バインド用パラメータ生成クラスのリストを取得します。
	 *
	 * @return 自動バインド用パラメータ生成クラスのリスト
	 */
	@Override
	public List<AutoBindParameterCreator> getAutoBindParameterCreators() {
		return autoBindParameterCreators;
	}

	/**
	 * 自動バインド用パラメータ生成クラスのリストを設定します。
	 *
	 * @param autoBindParameterCreators
	 *            自動バインド用パラメータ生成クラスのリスト
	 */
	@Override
	public void setAutoBindParameterCreators(final List<AutoBindParameterCreator> autoBindParameterCreators) {
		this.autoBindParameterCreators = autoBindParameterCreators;
	}

	/**
	 * 定数クラスパラメータMap生成
	 *
	 * @return 定数クラスパラメータMap
	 */
	private Map<? extends String, ? extends Parameter> buildConstParamMap() {
		Map<String, Parameter> paramMap = new HashMap<>();
		for (String className : constantClassNames) {
			try {
				Class<?> targetClass = Class.forName(className);
				makeConstParamMap(paramMap, targetClass);
			} catch (ClassNotFoundException ex) {
				LOG.error(ex.getMessage(), ex);
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
			for (Class<? extends Enum<?>> targetClass : listupEnumClasses(packageName)) {
				makeEnumConstParamMap(paramMap, packageName, targetClass);
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
		String resourceName = packageName.replace('.', '/');
		ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
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
				try (JarFile jarFile = ((JarURLConnection) root.openConnection()).getJarFile()) {
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
		try (Stream<Path> stream = Files.walk(dir)) {
			stream
					.filter(entry -> entry.getFileName().toString().endsWith(".class"))
					.forEach(file -> {
						StringJoiner joiner = new StringJoiner(".", packageName + ".", "");
						dir.relativize(file).forEach(p -> joiner.add(p.toString()));
						String className = joiner.toString().replaceAll(".class$", "");
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
	 * @param dir 対象ディレクトリ
	 * @return クラスリスト
	 * @throws ClassNotFoundException エラー
	 * @throws IOException
	 */
	private static Collection<? extends Class<?>> findEnumClassesWithJar(final String packageName, final JarFile jarFile) {
		String resourceName = packageName.replace('.', '/');
		Set<Class<?>> classes = new HashSet<>();
		Collections.list(jarFile.entries()).stream()
				.map(JarEntry::getName)
				.filter(name -> name.startsWith(resourceName))
				.filter(name -> name.endsWith(".class"))
				.map(name -> name.replace('/', '.').replaceAll(".class$", ""))
				.forEach(className -> loadEnum(className).ifPresent(classes::add));

		return classes;
	}

	/**
	 * Enumクラスをロード<br>
	 * 指定クラスがEnumでない場合はemptyを返す
	 *
	 * @param className
	 * @return ロードしたEnumクラス
	 */
	private static Optional<Class<?>> loadEnum(final String className) {
		try {
			Class<?> type = Class.forName(className);
			if (type.isEnum()) {
				return Optional.of(type);
			}
		} catch (ClassNotFoundException e) {
			LOG.error(e.getMessage(), e);
		}
		return Optional.empty();
	}
}
