package jp.co.future.uroborosql.context;

import java.io.InputStream;
import java.io.Reader;
import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLType;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.parameter.InOutParameter;
import jp.co.future.uroborosql.parameter.OutParameter;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parameter.ReaderParameter;
import jp.co.future.uroborosql.parameter.StreamParameter;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;
import jp.co.future.uroborosql.parser.TransformContext;
import jp.co.future.uroborosql.utils.BeanAccessor;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * SQLコンテキスト実装クラス
 *
 * @author H.Sugimoto
 */
public class SqlContextImpl implements SqlContext {
	/**
	 * @see #getParameterNames()
	 */
	private class ParameterNames extends AbstractSet<String> {

		@Override
		public Iterator<String> iterator() {
			return parameterMap.keySet().iterator();
		}

		@Override
		public int size() {
			return parameterMap.size();
		}

		@Override
		public boolean contains(final Object o) {
			return parameterMap.containsKey(o);
		}
	}

	/** where句の直後にくるANDやORを除外するための正規表現 */
	protected static final Pattern WHERE_CLAUSE_PATTERN = Pattern
			.compile("(?i)(WHERE\\s+(--.*|/\\*.*\\*/\\s*)*\\s*)(AND\\s+|OR\\s+)");

	/** 各句の最初に現れるカンマを除去するための正規表現 */
	protected static final Pattern REMOVE_FIRST_COMMA_PATTERN = Pattern
			.compile("(?i)(((SELECT|ORDER\\s+BY|GROUP\\s+BY|SET)\\s+|\\(\\s*)(--.*|/\\*.*\\*/\\s*)*\\s*)(,)");
	/** 不要な空白、改行を除去するための正規表現 */
	protected static final Pattern CLEAR_BLANK_PATTERN = Pattern.compile("(?m)^\\s*(\\r\\n|\\r|\\n)");

	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(SqlContextImpl.class);

	/** SQL名 */
	private String sqlName;

	/** 変換前のSQL文 */
	private String originalSql;

	/** 変換後のSQL文 */
	private final StringBuilder executableSql = new StringBuilder();

	/** 変換後のSQL文字列をキャッシュしたもの。 */
	private String executableSqlCache = "";

	/** SQL文の識別子 */
	private String sqlId;

	/** SQL実行の最大リトライ数 */
	private int maxRetryCount = 0;

	/** リトライを行う場合の待機時間（ms） */
	private int retryWaitTime = 0;

	/** パラメータ保持用マップ */
	private Map<String, Parameter> parameterMap = new HashMap<>();

	/** 定数パラメータ保持用マップ */
	private Map<String, Parameter> constParameterMap = null;

	/** SqlFilter管理クラス */
	private SqlFilterManager sqlFilterManager = new SqlFilterManagerImpl();

	/** バインド対象パラメータ名リスト */
	private final List<String> bindNames = new ArrayList<>();

	/** バインド変数リスト */
	private final List<Object> bindValiables = new ArrayList<>();

	/** 有効フラグ（BEGIN句で使用） */
	private boolean enabled = true;

	/** バッチ処理用パラメータ保持用マップリスト */
	private final List<Map<String, Parameter>> batchParameters = new ArrayList<>();

	/** 列の型の再定義保持用マップ */
	private final Map<Integer, Integer> defineColumnTypeMap = new HashMap<>();

	/** カーソルのタイプ(デフォルト値：カーソルは最初から最後まで順方向にしか移動できません。) */
	private int resultSetType = ResultSet.TYPE_FORWARD_ONLY;

	/** 変更可能性(デフォルト値：カーソルはデータの読み出ししかサポートしません。) */
	private int resultSetConcurrency = ResultSet.CONCUR_READ_ONLY;

	/** DBエイリアス名 */
	private String dbAlias = null;

	/** コンテキスト属性情報 */
	private final Map<String, Object> contextAttributes = new HashMap<>();

	/** 自動パラメータバインド関数 */
	private Consumer<SqlContext> autoParameterBinder = null;

	/** パラメータ変換マネージャ */
	private BindParameterMapperManager parameterMapperManager;

	private ParameterNames parameterNames;

	/**
	 * コンストラクタ。
	 */
	SqlContextImpl() {
	}

	/**
	 * コピーコンストラクタ
	 *
	 * @param parent コピー元のSQLコンテキスト
	 */
	private SqlContextImpl(final SqlContextImpl parent) {
		enabled = false;
		sqlId = parent.sqlId;
		sqlName = parent.sqlName;
		maxRetryCount = parent.maxRetryCount;
		retryWaitTime = parent.retryWaitTime;
		parameterMap = parent.parameterMap;
		constParameterMap = parent.constParameterMap;
		sqlFilterManager = parent.sqlFilterManager;
		batchParameters.addAll(parent.batchParameters);
		defineColumnTypeMap.putAll(parent.defineColumnTypeMap);
		resultSetType = parent.resultSetType;
		resultSetConcurrency = parent.resultSetConcurrency;
		dbAlias = parent.dbAlias;
		contextAttributes.putAll(parent.contextAttributes);
		autoParameterBinder = parent.autoParameterBinder;
		parameterMapperManager = parent.parameterMapperManager;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#copyTransformContext()
	 */
	@Override
	public TransformContext copyTransformContext() {
		return new SqlContextImpl(this);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#getExecutableSql()
	 */
	@Override
	public String getExecutableSql() {
		if (StringUtils.isEmpty(executableSqlCache)) {
			if (executableSql.length() > 0) {
				executableSqlCache = executableSql.toString();
				if (executableSqlCache.toUpperCase().contains("WHERE")) {
					// where句の直後に来るANDやORの除去
					StringBuffer buff = new StringBuffer();
					Matcher matcher = WHERE_CLAUSE_PATTERN.matcher(executableSqlCache);
					while (matcher.find()) {
						String whereClause = matcher.group(1);
						matcher.appendReplacement(buff, whereClause);
					}
					matcher.appendTail(buff);
					executableSqlCache = buff.toString();
				}
				// 各句の直後に現れる不要なカンマの除去
				StringBuffer buff = new StringBuffer();
				Matcher removeCommaMatcher = REMOVE_FIRST_COMMA_PATTERN.matcher(executableSqlCache);
				while (removeCommaMatcher.find()) {
					String clauseWords = removeCommaMatcher.group(1);
					removeCommaMatcher.appendReplacement(buff, clauseWords);
				}
				removeCommaMatcher.appendTail(buff);
				executableSqlCache = buff.toString();

				// 空行の除去
				executableSqlCache = CLEAR_BLANK_PATTERN.matcher(executableSqlCache).replaceAll("");
			}
		}
		return executableSqlCache;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#getSql()
	 */
	@Override
	public String getSql() {
		return originalSql;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#setSql(java.lang.String)
	 */
	@Override
	public SqlContext setSql(final String originalSql) {
		this.originalSql = originalSql;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#getSqlName()
	 */
	@Override
	public String getSqlName() {
		return sqlName;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#setSqlName(java.lang.String)
	 */
	@Override
	public SqlContext setSqlName(final String sqlName) {
		this.sqlName = sqlName;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#getSqlId()
	 */
	@Override
	public String getSqlId() {
		return sqlId;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#setSqlId(java.lang.String)
	 */
	@Override
	public SqlContext setSqlId(final String sqlId) {
		this.sqlId = sqlId;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#getMaxRetryCount()
	 */
	@Override
	public int getMaxRetryCount() {
		return maxRetryCount;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#setMaxRetryCount(int)
	 */
	@Override
	public SqlContext setMaxRetryCount(final int maxRetryCount) {
		this.maxRetryCount = maxRetryCount;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#getRetryWaitTime()
	 */
	@Override
	public int getRetryWaitTime() {
		return retryWaitTime;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#setRetryWaitTime(int)
	 */
	@Override
	public SqlContext setRetryWaitTime(final int retryWaitTime) {
		this.retryWaitTime = retryWaitTime;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#getParam(java.lang.String)
	 */
	@Override
	public Parameter getParam(final String paramName) {
		Parameter param = getBindParameter(paramName);
		if (param == null) {
			Map<String, Parameter> constParams = getConstParameterMap();
			if (constParams != null) {
				param = constParams.get(paramName.toUpperCase());
			}
		}
		return param;
	}

	/**
	 * parameterMapから指定されたキーのパラメータを取得する
	 *
	 * @param paramName パラメータ名
	 * @return パラメータ
	 */
	private Parameter getBindParameter(final String paramName) {
		String[] keys = StringUtils.split(paramName, ".");
		String baseName = keys[0];

		Parameter parameter = parameterMap.get(baseName);
		if (parameter == null) {
			return null;
		}

		if (keys.length > 1) {
			String propertyName = keys[1];
			return parameter.createSubParameter(propertyName);
		}

		return parameter;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#context()
	 */
	@Override
	public SqlContext context() {
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#hasParam(java.lang.String)
	 */
	@Override
	public boolean hasParam(final String paramName) {
		return parameterMap.containsKey(paramName);
	}

	/**
	 * パラメータの追加
	 *
	 * @param parameter パラメータ
	 *
	 * @return SqlContext
	 */
	private SqlContext param(final Parameter parameter) {
		parameterMap.put(parameter.getParameterName(), parameter);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlContext param(final String parameterName, final Object value) {
		return param(new Parameter(parameterName, value));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfAbsent(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlContext paramIfAbsent(final String parameterName, final Object value) {
		if (!hasParam(parameterName)) {
			param(parameterName, value);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramList(String, Object...)
	 */
	@Override
	public SqlContext paramList(final String parameterName, final Object... value) {
		return param(new Parameter(parameterName, Arrays.asList(value)));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramListIfAbsent(String, Object...)
	 */
	@Override
	public SqlContext paramListIfAbsent(final String parameterName, final Object... value) {
		if (!hasParam(parameterName)) {
			paramList(parameterName, value);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramMap(java.util.Map)
	 */
	@Override
	public SqlContext paramMap(final Map<String, ?> paramMap) {
		if (paramMap != null) {
			paramMap.forEach((k, v) -> param(k, v));
		}
		return this;
	}

	@Override
	public SqlContext paramBean(final Object bean) {
		if (bean != null) {
			BeanAccessor.fields(bean.getClass()).stream()
					.forEach(f -> param(f.getName(), BeanAccessor.value(f, bean)));
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public SqlContext param(final String parameterName, final Object value, final SQLType sqlType) {
		return param(new Parameter(parameterName, value, sqlType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfAbsent(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public SqlContext paramIfAbsent(final String parameterName, final Object value, final SQLType sqlType) {
		if (!hasParam(parameterName)) {
			param(parameterName, value, sqlType);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public SqlContext param(final String parameterName, final Object value, final int sqlType) {
		return param(new Parameter(parameterName, value, sqlType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfAbsent(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public SqlContext paramIfAbsent(final String parameterName, final Object value, final int sqlType) {
		if (!hasParam(parameterName)) {
			param(parameterName, value, sqlType);
		}
		return this;
	}

	/**
	 * パラメータ名のリストを取得する
	 *
	 * @return パラメータ名のリスト
	 */
	public Set<String> getParameterNames() {
		return parameterNames != null ? parameterNames : (parameterNames = new ParameterNames());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#outParam(java.lang.String, java.sql.SQLType)
	 */
	@Override
	public SqlContext outParam(final String parameterName, final SQLType sqlType) {
		return param(new OutParameter(parameterName, sqlType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#outParam(java.lang.String, int)
	 */
	@Override
	public SqlContext outParam(final String parameterName, final int sqlType) {
		return param(new OutParameter(parameterName, sqlType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParam(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public SqlContext inOutParam(final String parameterName, final Object value, final SQLType sqlType) {
		return param(new InOutParameter(parameterName, value, sqlType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParamIfAbsent(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public SqlContext inOutParamIfAbsent(final String parameterName, final Object value, final SQLType sqlType) {
		if (!hasParam(parameterName)) {
			inOutParam(parameterName, value, sqlType);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParam(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public SqlContext inOutParam(final String parameterName, final Object value, final int sqlType) {
		return param(new InOutParameter(parameterName, value, sqlType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParamIfAbsent(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public SqlContext inOutParamIfAbsent(final String parameterName, final Object value, final int sqlType) {
		if (!hasParam(parameterName)) {
			inOutParam(parameterName, value, sqlType);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParam(java.lang.String, java.io.InputStream)
	 */
	@Override
	public SqlContext blobParam(final String parameterName, final InputStream value) {
		return param(new StreamParameter(parameterName, value));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParamIfAbsent(java.lang.String, java.io.InputStream)
	 */
	@Override
	public SqlContext blobParamIfAbsent(final String parameterName, final InputStream value) {
		if (!hasParam(parameterName)) {
			blobParam(parameterName, value);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParam(java.lang.String, java.io.InputStream, int)
	 */
	@Override
	public SqlContext blobParam(final String parameterName, final InputStream value, final int len) {
		return param(new StreamParameter(parameterName, value, len));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParamIfAbsent(java.lang.String, java.io.InputStream, int)
	 */
	@Override
	public SqlContext blobParamIfAbsent(final String parameterName, final InputStream value, final int len) {
		if (!hasParam(parameterName)) {
			blobParam(parameterName, value, len);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParam(java.lang.String, java.io.Reader)
	 */
	@Override
	public SqlContext clobParam(final String paramName, final Reader value) {
		return param(new ReaderParameter(paramName, value));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParamIfAbsent(java.lang.String, java.io.Reader)
	 */
	@Override
	public SqlContext clobParamIfAbsent(final String paramName, final Reader value) {
		if (!hasParam(paramName)) {
			clobParam(paramName, value);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParam(java.lang.String, java.io.Reader, int)
	 */
	@Override
	public SqlContext clobParam(final String paramName, final Reader value, final int len) {
		return param(new ReaderParameter(paramName, value, len));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParamIfAbsent(java.lang.String, java.io.Reader, int)
	 */
	@Override
	public SqlContext clobParamIfAbsent(final String paramName, final Reader value, final int len) {
		if (!hasParam(paramName)) {
			clobParam(paramName, value, len);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#retry(int)
	 */
	@Override
	public SqlContext retry(final int count) {
		return retry(count, 0);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#retry(int, int)
	 */
	@Override
	public SqlContext retry(final int count, final int waitTime) {
		return this.setMaxRetryCount(count).setRetryWaitTime(waitTime);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#addSqlPart(java.lang.String)
	 */
	@Override
	public TransformContext addSqlPart(final String sqlPart) {
		executableSql.append(sqlPart);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#isEnabled()
	 */
	@Override
	public boolean isEnabled() {
		return enabled;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#setEnabled(boolean)
	 */
	@Override
	public void setEnabled(final boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#addBindName(java.lang.String)
	 */
	@Override
	public TransformContext addBindName(final String bindName) {
		bindNames.add(bindName);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#addBindNames(java.util.List)
	 */
	@Override
	public TransformContext addBindNames(final List<String> bindNames) {
		this.bindNames.addAll(bindNames);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#getBindNames()
	 */
	@Override
	public List<String> getBindNames() {
		return bindNames;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#addBindVariable(java.lang.Object)
	 */
	@Override
	public TransformContext addBindVariable(final Object bindValiable) {
		bindValiables.add(bindValiable);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#addBindVariables(java.lang.Object[])
	 */
	@Override
	public TransformContext addBindVariables(final Object[] bindValiables) {
		for (Object bindValiable : bindValiables) {
			this.bindValiables.add(bindValiable);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.TransformContext#getBindVariables()
	 */
	@Override
	public Object[] getBindVariables() {
		return bindValiables.toArray();
	}

	/**
	 * バインドパラメータ配列取得
	 *
	 * @return バインドパラメータ配列
	 */
	public Parameter[] getBindParameters() {
		return bindNames.stream().map(this::getParam).filter(Objects::nonNull).toArray(Parameter[]::new);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#bindParams(java.sql.PreparedStatement)
	 */
	@Override
	public void bindParams(final PreparedStatement preparedStatement) throws SQLException {
		Parameter[] bindParameters = getBindParameters();

		Set<String> matchParams = new HashSet<>();
		int parameterIndex = 1;
		for (Parameter bindParameter : bindParameters) {
			Parameter parameter = getSqlFilterManager().doParameter(bindParameter);
			parameterIndex = parameter.setParameter(preparedStatement, parameterIndex, parameterMapperManager);
			matchParams.add(parameter.getParameterName());
		}
		// SQL上のバインドパラメータ群（bindNames）に対応する値がすべて設定されているかどうかをチェックする
		if (!matchParams.containsAll(bindNames)) {
			Set<String> missMatchParams = new LinkedHashSet<>(bindNames);
			missMatchParams.removeAll(matchParams);
			throw new ParameterNotFoundRuntimeException("Parameter " + missMatchParams.toString() + " is not bound.");
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#bindBatchParams(java.sql.PreparedStatement)
	 */
	@Override
	public void bindBatchParams(final PreparedStatement preparedStatement) throws SQLException {
		for (Map<String, Parameter> paramMap : batchParameters) {
			parameterMap = paramMap;
			bindParams(preparedStatement);
			preparedStatement.addBatch();
		}
		LOG.debug("{} items Added for batch process.", batchParameters.size());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#getOutParams(java.sql.CallableStatement)
	 */
	@Override
	public Map<String, Object> getOutParams(final CallableStatement callableStatement) throws SQLException {
		Map<String, Object> out = new HashMap<>();
		Parameter[] bindParameters = getBindParameters();
		int parameterIndex = 1;
		for (Parameter parameter : bindParameters) {
			if (parameter instanceof OutParameter) {
				String key = parameter.getParameterName();
				out.put(key, getSqlFilterManager().doOutParameter(key, callableStatement.getObject(parameterIndex)));
			}
			parameterIndex++;
		}

		LOG.debug("Stored procedure out parameter[{}]", out);
		return out;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#addBatch()
	 */
	@Override
	public SqlContext addBatch() {
		acceptAutoParameterBinder();
		batchParameters.add(parameterMap);
		parameterMap = new HashMap<>();
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#clearBatch()
	 */
	@Override
	public SqlContext clearBatch() {
		batchParameters.clear();
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#batchCount()
	 */
	@Override
	public int batchCount() {
		return batchParameters.size();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#acceptAutoParameterBinder()
	 */
	@Override
	public void acceptAutoParameterBinder() {
		if (autoParameterBinder != null) {
			autoParameterBinder.accept(this);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#addDefineColumnType(int, int)
	 */
	@Override
	public void addDefineColumnType(final int column, final int type) {
		defineColumnTypeMap.put(column, type);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#getDefineColumnTypes()
	 */
	@Override
	public Map<Integer, Integer> getDefineColumnTypes() {
		return defineColumnTypeMap;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#setResultSetType(int)
	 */
	@Override
	public void setResultSetType(final int resultSetType) {
		this.resultSetType = resultSetType;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#getResultSetType()
	 */
	@Override
	public int getResultSetType() {
		return resultSetType;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#setResultSetConcurrency(int)
	 */
	@Override
	public void setResultSetConcurrency(final int resultSetConcurrency) {
		this.resultSetConcurrency = resultSetConcurrency;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#getResultSetConcurrency()
	 */
	@Override
	public int getResultSetConcurrency() {
		return resultSetConcurrency;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#setDBAlias(java.lang.String)
	 */
	@Override
	public void setDBAlias(final String dbAlias) {
		this.dbAlias = dbAlias;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#getDbAlias()
	 */
	@Override
	public String getDbAlias() {
		return dbAlias;
	}

	/**
	 * 定数パラメータマップの取得
	 *
	 * @return 定数パラメータマップ
	 */
	public Map<String, Parameter> getConstParameterMap() {
		return constParameterMap;
	}

	/**
	 * 定数パラメータマップの設定
	 *
	 * @param constParameterMap 定数パラメータマップ
	 */
	public void setConstParameterMap(final Map<String, Parameter> constParameterMap) {
		this.constParameterMap = constParameterMap;
	}

	/**
	 * SqlFilter管理クラスを取得します。
	 *
	 * @return SqlFilter管理クラス
	 */
	public SqlFilterManager getSqlFilterManager() {
		return sqlFilterManager;
	}

	/**
	 * SqlFilter管理クラスを設定します。
	 *
	 * @param sqlFilterManager SQLフィルタ管理クラス SqlFilter管理クラス
	 */
	public void setSqlFilterManager(final SqlFilterManager sqlFilterManager) {
		this.sqlFilterManager = sqlFilterManager;
	}

	/**
	 * パラメータ変換マネージャを取得します
	 *
	 * @return パラメータ変換マネージャ
	 */
	public BindParameterMapperManager getParameterMapperManager() {
		return parameterMapperManager;
	}

	/**
	 * パラメータ変換マネージャを設定します
	 *
	 * @param parameterMapperManager パラメータ変換マネージャ
	 */
	public void setParameterMapperManager(final BindParameterMapperManager parameterMapperManager) {
		this.parameterMapperManager = parameterMapperManager;
	}

	/**
	 * 自動パラメータバインド関数を設定します
	 * @param binder 自動パラメータバインド関数
	 */
	public void setAutoParameterBinder(final Consumer<SqlContext> binder) {
		this.autoParameterBinder = binder;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.context.SqlContext#contextAttrs()
	 */
	@Override
	public Map<String, Object> contextAttrs() {
		return contextAttributes;
	}

	@Override
	public String formatParams() {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < bindNames.size(); i++) {
			sb.append(String.format("[%s=%s]", bindNames.get(i), bindValiables.get(i)));
		}
		return sb.toString();
	}

}
