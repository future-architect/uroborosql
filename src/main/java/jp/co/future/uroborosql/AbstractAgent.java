package jp.co.future.uroborosql;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.coverage.CoverageData;
import jp.co.future.uroborosql.coverage.CoverageHandler;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.fluent.SqlEntityQuery;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.fluent.SqlUpdate;
import jp.co.future.uroborosql.mapping.DefaultEntityHandler;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.parser.ContextTransformer;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;
import jp.co.future.uroborosql.store.SqlManager;
import jp.co.future.uroborosql.tx.LocalTransactionManager;
import jp.co.future.uroborosql.tx.SQLRunnable;
import jp.co.future.uroborosql.tx.SQLSupplier;
import jp.co.future.uroborosql.tx.TransactionManager;
import jp.co.future.uroborosql.utils.StringFunction;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * SqlAgentの抽象親クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractAgent implements SqlAgent {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(AbstractAgent.class);

	/** SQLカバレッジ用ロガー */
	protected static final Logger COVERAGE_LOG = LoggerFactory.getLogger(SqlAgent.class.getPackage().getName()
			+ "sql.coverage");

	/** 文字列用関数(OGNLで利用) */
	protected static final StringFunction SF = new StringFunction();

	/** ログ出力を抑止するためのMDCキー */
	protected static final String SUPPRESS_PARAMETER_LOG_OUTPUT = "SuppressParameterLogOutput";

	/** カバレッジハンドラ */
	private static AtomicReference<CoverageHandler> coverageHandlerRef = new AtomicReference<>();

	/** SQL設定管理クラス */
	protected SqlConfig sqlConfig;

	/** トランザクション管理機能 */
	protected TransactionManager transactionManager;

	/** SQLマネージャ */
	protected SqlManager sqlManager;

	/** SqlFilter管理クラス */
	private final SqlFilterManager sqlFilterManager;

	/** ORM処理クラス */
	private final EntityHandler<?> entityHandler;

	/** 終端文字（;）を削除するかどうか。デフォルト値は<code>true</code> */
	private boolean removeTerminator = true;

	/** クエリータイムアウト制限値 */
	private int queryTimeout = -1;

	/** フェッチサイズ */
	private int fetchSize = -1;

	/** SQL実行エラー時にリトライするエラーコードのリスト */
	private List<String> sqlRetryCodes = Collections.emptyList();

	/** SQL実行エラー時の最大リトライ回数 */
	private int maxRetryCount = 0;

	/** SQL実行リトライ時の待機時間(ms) */
	private int retryWaitTime = 0;

	/** SQLを特定するための一意なIDに置換するためのキー */
	private String keySqlId = "_SQL_ID_";

	static {
		// SQLカバレッジ取得用のクラス名を設定する。指定がない場合、またはfalseが指定された場合はカバレッジを収集しない。
		// クラス名が指定されている場合はそのクラス名を指定
		String sqlCoverageClassName = System.getProperty(KEY_SQL_COVERAGE);
		if (sqlCoverageClassName == null || Boolean.FALSE.toString().equalsIgnoreCase(sqlCoverageClassName)) {
			sqlCoverageClassName = null;
		} else if (Boolean.TRUE.toString().equalsIgnoreCase(sqlCoverageClassName)) {
			// trueの場合は、デフォルト値を設定
			sqlCoverageClassName = AbstractAgent.class.getPackage().getName() + ".coverage.CoberturaCoverageHandler";
		}

		CoverageHandler handler = null;
		if (sqlCoverageClassName != null) {
			try {
				handler = (CoverageHandler) Class.forName(sqlCoverageClassName, true,
						Thread.currentThread().getContextClassLoader()).newInstance();
			} catch (Exception ex) {
				LOG.warn("Failed to generate CoverageHandler class. Class：{}, Cause：{}", sqlCoverageClassName,
						ex.getMessage());
			}
		}

		if (handler != null) {
			coverageHandlerRef.set(handler);
		}
	}

	/**
	 * コンストラクタ。
	 *
	 * @param connectionSupplier コネクション供給クラス
	 * @param sqlManager SQL管理クラス
	 * @param sqlFilterManager SQLフィルタ管理クラス
	 * @param defaultProps デフォルト値プロパティ
	 */
	public AbstractAgent(final ConnectionSupplier connectionSupplier, final SqlManager sqlManager,
			final SqlFilterManager sqlFilterManager, final Map<String, String> defaultProps) {
		this(connectionSupplier, sqlManager, sqlFilterManager, null, defaultProps);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param connectionSupplier コネクション供給クラス
	 * @param sqlManager SQL管理クラス
	 * @param sqlFilterManager SQLフィルタ管理クラス
	 * @param entityHandler ORM処理クラス
	 * @param defaultProps デフォルト値プロパティ
	 */
	public AbstractAgent(final ConnectionSupplier connectionSupplier, final SqlManager sqlManager,
			final SqlFilterManager sqlFilterManager, final EntityHandler<?> entityHandler,
			final Map<String, String> defaultProps) {
		transactionManager = new LocalTransactionManager(connectionSupplier, sqlFilterManager);
		this.sqlManager = sqlManager;
		this.sqlFilterManager = sqlFilterManager;
		this.entityHandler = entityHandler != null ? entityHandler : new DefaultEntityHandler();

		// デフォルトプロパティ設定
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_REMOVE_TERMINATOR)) {
			removeTerminator = Boolean.parseBoolean(defaultProps.get(SqlAgentFactory.PROPS_KEY_REMOVE_TERMINATOR));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_FETCH_SIZE)) {
			fetchSize = Integer.parseInt(defaultProps.get(SqlAgentFactory.PROPS_KEY_FETCH_SIZE));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_QUERY_TIMEOUT)) {
			queryTimeout = Integer.parseInt(defaultProps.get(SqlAgentFactory.PROPS_KEY_QUERY_TIMEOUT));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_SQL_RETRY_CODES)) {
			sqlRetryCodes = Collections.unmodifiableList(Arrays.asList(defaultProps.get(
					SqlAgentFactory.PROPS_KEY_SQL_RETRY_CODES).split(",")));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_DEFAULT_MAX_RETRY_COUNT)) {
			maxRetryCount = Integer.parseInt(defaultProps.get(SqlAgentFactory.PROPS_KEY_DEFAULT_MAX_RETRY_COUNT));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME)) {
			retryWaitTime = Integer.parseInt(defaultProps.get(SqlAgentFactory.PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_SQL_ID_KEY_NAME)) {
			keySqlId = defaultProps.get(SqlAgentFactory.PROPS_KEY_SQL_ID_KEY_NAME);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#getConnection()
	 */
	@Override
	public Connection getConnection() {
		return transactionManager.getConnection();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#getConnection(java.lang.String)
	 */
	@Override
	public Connection getConnection(final String alias) {
		return transactionManager.getConnection(alias);
	}

	/**
	 * SQLマネージャを取得します。
	 *
	 * @return SQLマネージャ
	 */
	protected SqlManager getSqlManager() {
		return sqlManager;
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
	 * ORM処理クラス を取得します。
	 *
	 * @return ORM処理クラス
	 */
	protected EntityHandler<?> getEntityHandler() {
		return entityHandler;
	}

	/**
	 * SqlContextの設定内容を元にSQLを構築する
	 *
	 * @param sqlContext SQLコンテキスト
	 */
	protected void transformContext(final SqlContext sqlContext) {
		String originalSql = sqlContext.getSql();
		if (StringUtils.isEmpty(originalSql) && getSqlManager() != null) {
			originalSql = getSqlManager().getSql(sqlContext.getSqlName());
			if (StringUtils.isEmpty(originalSql)) {
				throw new UroborosqlRuntimeException("指定されたSQLファイル[" + sqlContext.getSqlName() + "]が見つかりません。");
			}
			originalSql = getSqlFilterManager().doTransformSql(sqlContext, originalSql);
			sqlContext.setSql(originalSql);
		}

		// SQL-IDの付与
		if (originalSql.contains(keySqlId)) {
			String sqlId = sqlContext.getSqlId();
			if (StringUtils.isEmpty(sqlId)) {
				sqlId = sqlContext.getSqlName();
			}
			originalSql = originalSql.replace(keySqlId, sqlId);
		}

		// ユーザファンクション登録
		if (sqlContext.getParam(StringFunction.SHORT_NAME) == null) {
			sqlContext.param(StringFunction.SHORT_NAME, SF);
		}

		if (StringUtils.isEmpty(sqlContext.getExecutableSql())) {
			SqlParser sqlParser = new SqlParserImpl(originalSql, isRemoveTerminator());
			ContextTransformer contextTransformer = sqlParser.parse();
			contextTransformer.transform(sqlContext);

			if (coverageHandlerRef.get() != null) {
				// SQLカバレッジ用のログを出力する
				CoverageData coverageData = new CoverageData(sqlContext.getSqlName(), originalSql,
						contextTransformer.getPassedRoute());
				COVERAGE_LOG.trace("{}", coverageData);

				coverageHandlerRef.get().accept(coverageData);
			}
		}

		LOG.trace("Template SQL[{}{}{}]", System.lineSeparator(), originalSql, System.lineSeparator());
		LOG.debug("Executed SQL[{}{}{}]", System.lineSeparator(), sqlContext.getExecutableSql(), System.lineSeparator());
	}

	/**
	 * フェッチサイズとクエリタイムアウトをPreparedStatementに設定する
	 *
	 * @param preparedStatement PreparedStatement
	 * @throws SQLException SQL例外
	 */
	protected void applyProperties(final PreparedStatement preparedStatement) throws SQLException {
		// フェッチサイズ指定
		if (getFetchSize() >= 0) {
			preparedStatement.setFetchSize(getFetchSize());
		}

		// クエリタイムアウト指定
		if (getQueryTimeout() >= 0) {
			preparedStatement.setQueryTimeout(getQueryTimeout());
		}
	}

	/**
	 * 例外発生時ハンドラー
	 *
	 * @param sqlContext SQLコンテキスト
	 * @param ex SQL例外
	 * @throws SQLException SQL例外
	 */
	protected abstract void handleException(SqlContext sqlContext, SQLException ex) throws SQLException;

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#close()
	 */
	@Override
	public void close() throws SQLException {
		transactionManager.close();
		if (coverageHandlerRef.get() != null) {
			coverageHandlerRef.get().onSqlAgentClose();
		}
	}

	/**
	 * {@inheritDoc} {@link #close()} を呼び出す
	 *
	 * @see java.lang.Object#finalize()
	 */
	@Override
	protected void finalize() throws Throwable {
		close();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void required(final SQLRunnable runnable) throws SQLException {
		transactionManager.required(runnable);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R required(final SQLSupplier<R> supplier) throws SQLException {
		return transactionManager.required(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void requiresNew(final SQLRunnable runnable) throws SQLException {
		transactionManager.requiresNew(runnable);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R requiresNew(final SQLSupplier<R> supplier) throws SQLException {
		return transactionManager.requiresNew(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void notSupported(final SQLRunnable runnable) throws SQLException {
		transactionManager.notSupported(runnable);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R notSupported(final SQLSupplier<R> supplier) throws SQLException {
		return transactionManager.notSupported(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#setRollbackOnly()
	 */
	@Override
	public void setRollbackOnly() {
		transactionManager.setRollbackOnly();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#setSavepoint(java.lang.String)
	 */
	@Override
	public void setSavepoint(final String savepointName) throws SQLException {
		transactionManager.setSavepoint(savepointName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#releaseSavepoint(java.lang.String)
	 */
	@Override
	public void releaseSavepoint(final String savepointName) throws SQLException {
		transactionManager.releaseSavepoint(savepointName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#rollback(java.lang.String)
	 */
	@Override
	public void rollback(final String savepointName) throws SQLException {
		transactionManager.rollback(savepointName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#rollback()
	 */
	@Override
	public void rollback() throws SQLException {
		transactionManager.rollback();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#commit()
	 */
	@Override
	public void commit() throws SQLException {
		transactionManager.commit();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#getSqlConfig()
	 */
	@Override
	public SqlConfig getSqlConfig() {
		return this.sqlConfig;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#setSqlConfig(jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public void setSqlConfig(final SqlConfig config) {
		this.sqlConfig = config;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#context()
	 */
	@Override
	public SqlContext context() {
		return sqlConfig.context();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#context()
	 */
	@Override
	public SqlContext contextFrom(final String sqlName) {
		return sqlConfig.context().setSqlName(sqlName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#context()
	 */
	@Override
	public SqlContext contextWith(final String sql) {
		return sqlConfig.context().setSql(sql);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(java.lang.String)
	 */
	@Override
	public SqlQuery query(final String sqlName) {
		SqlContext context = contextFrom(sqlName);
		return new SqlQueryImpl(this, context);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#queryWith(java.lang.String)
	 */
	@Override
	public SqlQuery queryWith(final String sql) {
		SqlContext context = contextWith(sql);
		return new SqlQueryImpl(this, context);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#update(java.lang.String)
	 */
	@Override
	public SqlUpdate update(final String sqlName) {
		SqlContext context = contextFrom(sqlName);
		return new SqlUpdateImpl(this, context);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updateWith(java.lang.String)
	 */
	@Override
	public SqlUpdate updateWith(final String sql) {
		SqlContext context = contextWith(sql);
		return new SqlUpdateImpl(this, context);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#proc(java.lang.String)
	 */
	@Override
	public Procedure proc(final String sqlName) {
		SqlContext context = contextFrom(sqlName);
		return new ProcedureImpl(this, context);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#procWith(java.lang.String)
	 */
	@Override
	public Procedure procWith(final String sql) {
		SqlContext context = contextWith(sql);
		return new ProcedureImpl(this, context);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <E> SqlEntityQuery<E> query(final Class<? extends E> entityType) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			TableMetadata metadata = handler.getMetadata(this.transactionManager, entityType);

			SqlContext context = handler.createSelectContext(this, metadata, entityType);

			return new SqlEntityQueryImpl<>(this, handler, context, entityType);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(EntitySqlRuntimeException.EntityProcKind.SELECT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#getFetchSize()
	 */
	@Override
	public int getFetchSize() {
		return fetchSize;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#setFetchSize(int)
	 */
	@Override
	public void setFetchSize(final int fetchSize) {
		this.fetchSize = fetchSize;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#getQueryTimeout()
	 */
	@Override
	public int getQueryTimeout() {
		return queryTimeout;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#setQueryTimeout(int)
	 */
	@Override
	public void setQueryTimeout(final int queryTimeout) {
		this.queryTimeout = queryTimeout;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#isRemoveTerminator()
	 */
	@Override
	public boolean isRemoveTerminator() {
		return removeTerminator;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#setRemoveTerminator(boolean)
	 */
	@Override
	public void setRemoveTerminator(final boolean removeTerminator) {
		this.removeTerminator = removeTerminator;
	}

	/**
	 * SQL実行をリトライするSQLエラーコードのリスト を取得します
	 *
	 * @return SQL実行をリトライするSQLエラーコードのリスト
	 */
	public List<String> getSqlRetryCodes() {
		return sqlRetryCodes;
	}

	/**
	 * SQL実行をリトライするSQLエラーコードのリスト を設定します
	 *
	 * @param sqlRetryCodes SQL実行をリトライするSQLエラーコードのリスト
	 */
	public void setSqlRetryCodes(final List<String> sqlRetryCodes) {
		this.sqlRetryCodes = sqlRetryCodes;
	}

	/**
	 * 最大リトライ回数 を取得します
	 *
	 * @return 最大リトライ回数
	 */
	public int getMaxRetryCount() {
		return maxRetryCount;
	}

	/**
	 * 最大リトライ回数 を設定します
	 *
	 * @param maxRetryCount 最大リトライ回数
	 */
	public void setMaxRetryCount(final int maxRetryCount) {
		this.maxRetryCount = maxRetryCount;
	}

	/**
	 * リトライタイムアウト時間(ms) を取得します
	 *
	 * @return リトライタイムアウト時間(ms)
	 */
	public int getRetryWaitTime() {
		return retryWaitTime;
	}

	/**
	 * リトライタイムアウト時間(ms) を設定します
	 *
	 * @param retryWaitTime リトライタイムアウト時間(ms)
	 */
	public void setRetryWaitTime(final int retryWaitTime) {
		this.retryWaitTime = retryWaitTime;
	}

	/**
	 * ステートメント初期化。
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return PreparedStatement
	 * @throws SQLException SQL例外
	 */
	protected PreparedStatement getPreparedStatement(final SqlContext sqlContext) throws SQLException {
		PreparedStatement stmt = ((LocalTransactionManager) transactionManager).getPreparedStatement(sqlContext);
		// プロパティ設定
		applyProperties(stmt);
		return stmt;
	}

	/**
	 * Callableステートメント初期化
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return CallableStatement
	 * @throws SQLException SQL例外
	 */
	protected CallableStatement getCallableStatement(final SqlContext sqlContext) throws SQLException {
		CallableStatement stmt = ((LocalTransactionManager) transactionManager).getCallableStatement(sqlContext);
		// プロパティ設定
		applyProperties(stmt);
		return stmt;
	}
}
