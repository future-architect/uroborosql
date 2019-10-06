/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.coverage.CoverageData;
import jp.co.future.uroborosql.coverage.CoverageHandler;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.fluent.SqlBatch;
import jp.co.future.uroborosql.fluent.SqlEntityQuery;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.fluent.SqlUpdate;
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
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.StringFunction;

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

	/** ログ出力を抑止するためのMDCキー */
	protected static final String SUPPRESS_PARAMETER_LOG_OUTPUT = "SuppressParameterLogOutput";

	/** SqlContext属性キー:リトライカウント */
	protected static final String CTX_ATTR_KEY_RETRY_COUNT = "__retryCount";

	/** SqlContext属性キー:バインドパラメータコメントの出力有無 */
	protected static final String CTX_ATTR_KEY_OUTPUT_BIND_COMMENT = "__outputBindComment";

	/** 例外発生にロールバックが必要なDBでリトライを実現するために設定するSavepointの名前 */
	protected static final String RETRY_SAVEPOINT_NAME = "__retry_savepoint";

	/** バッチカウントの初期値 */
	protected static final int DEFAULT_BATCH_COUNT = 1000;

	/** 一括INSERT用のバッチフレームの判定条件 */
	protected static final InsertsCondition<Object> DEFAULT_INSERTS_WHEN_CONDITION = (context, count,
			row) -> count == DEFAULT_BATCH_COUNT;

	/** 一括UPDATE用のバッチフレームの判定条件 */
	protected static final UpdatesCondition<Object> DEFAULT_UPDATES_WHEN_CONDITION = (context, count,
			row) -> count == DEFAULT_BATCH_COUNT;

	/** カバレッジハンドラ */
	protected static AtomicReference<CoverageHandler> coverageHandlerRef = new AtomicReference<>();

	/** SQL設定管理クラス */
	protected SqlConfig sqlConfig;

	/** トランザクション管理機能 */
	protected TransactionManager transactionManager;

	/** クエリータイムアウト制限値 */
	protected int queryTimeout = -1;

	/** フェッチサイズ */
	protected int fetchSize = -1;

	/** SQL実行エラー時にリトライするエラーコードのリスト */
	protected List<String> sqlRetryCodes = Collections.emptyList();

	/** SQL実行エラー時の最大リトライ回数 */
	protected int maxRetryCount = 0;

	/** SQL実行リトライ時の待機時間(ms) */
	protected int retryWaitTime = 0;

	/** SQLを特定するための一意なIDに置換するためのキー */
	protected String keySqlId = "_SQL_ID_";

	/** Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat */
	protected CaseFormat defaultMapKeyCaseFormat = CaseFormat.UPPER_SNAKE_CASE;

	/** デフォルトの{@link InsertsType} */
	protected InsertsType defaultInsertsType = InsertsType.BULK;

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
				LOG.warn("Failed to generate CoverageHandler class. Class:{}, Cause:{}", sqlCoverageClassName,
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
	 * @param sqlConfig SQL設定管理クラス
	 * @param defaultProps デフォルト値プロパティ
	 */
	protected AbstractAgent(final SqlConfig sqlConfig, final Map<String, String> defaultProps) {
		this.sqlConfig = sqlConfig;
		this.transactionManager = new LocalTransactionManager(sqlConfig);

		// デフォルトプロパティ設定
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_FETCH_SIZE)) {
			this.fetchSize = Integer.parseInt(defaultProps.get(SqlAgentFactory.PROPS_KEY_FETCH_SIZE));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_QUERY_TIMEOUT)) {
			this.queryTimeout = Integer.parseInt(defaultProps.get(SqlAgentFactory.PROPS_KEY_QUERY_TIMEOUT));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_SQL_RETRY_CODES)) {
			this.sqlRetryCodes = Collections.unmodifiableList(Arrays.asList(defaultProps.get(
					SqlAgentFactory.PROPS_KEY_SQL_RETRY_CODES).split(",")));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_DEFAULT_MAX_RETRY_COUNT)) {
			this.maxRetryCount = Integer.parseInt(defaultProps.get(SqlAgentFactory.PROPS_KEY_DEFAULT_MAX_RETRY_COUNT));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME)) {
			this.retryWaitTime = Integer.parseInt(defaultProps
					.get(SqlAgentFactory.PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_SQL_ID_KEY_NAME)) {
			this.keySqlId = defaultProps.get(SqlAgentFactory.PROPS_KEY_SQL_ID_KEY_NAME);
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_DEFAULT_MAP_KEY_CASE_FORMAT)) {
			this.defaultMapKeyCaseFormat = CaseFormat.valueOf(defaultProps
					.get(SqlAgentFactory.PROPS_KEY_DEFAULT_MAP_KEY_CASE_FORMAT));
		}
		if (defaultProps.containsKey(SqlAgentFactory.PROPS_KEY_DEFAULT_INSERTS_TYPE)) {
			this.defaultInsertsType = InsertsType.valueOf(defaultProps
					.get(SqlAgentFactory.PROPS_KEY_DEFAULT_INSERTS_TYPE));
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
		return sqlConfig.getSqlManager();
	}

	/**
	 * SqlFilter管理クラスを取得します。
	 *
	 * @return SqlFilter管理クラス
	 */
	public SqlFilterManager getSqlFilterManager() {
		return sqlConfig.getSqlFilterManager();
	}

	/**
	 * ORM処理クラス を取得します。
	 *
	 * @return ORM処理クラス
	 */
	protected EntityHandler<?> getEntityHandler() {
		return sqlConfig.getEntityHandler();
	}

	/**
	 * SqlContextの設定内容を元にSQLを構築する
	 *
	 * @param sqlContext SQLコンテキスト
	 * @param isQuery queryかどうか。queryの場合<code>true</code>
	 */
	protected void transformContext(final SqlContext sqlContext, final boolean isQuery) {
		String originalSql = sqlContext.getSql();
		if (StringUtils.isEmpty(originalSql) && getSqlManager() != null) {
			originalSql = getSqlManager().getSql(sqlContext.getSqlName());
			if (StringUtils.isEmpty(originalSql)) {
				throw new UroborosqlRuntimeException("sql file:[" + sqlContext.getSqlName() + "] is not found.");
			}
		}
		originalSql = getSqlFilterManager().doTransformSql(sqlContext, originalSql);
		sqlContext.setSql(originalSql);

		// SQL-IDの付与
		if (originalSql.contains(keySqlId)) {
			String sqlId = sqlContext.getSqlId();
			if (StringUtils.isEmpty(sqlId)) {
				sqlId = sqlContext.getSqlName();
			}
			if (StringUtils.isEmpty(sqlId)) {
				sqlId = String.valueOf(originalSql.hashCode());
			}

			originalSql = originalSql.replace(keySqlId, sqlId);
		}

		// ユーザファンクション登録
		if (sqlContext.getParam(StringFunction.SHORT_NAME) == null) {
			sqlContext.param(StringFunction.SHORT_NAME, getSqlConfig().getDialect().getExpressionFunction());
		}
		sqlContext.param(AbstractExtractionCondition.PARAM_KEY_ESCAPE_CHAR,
				getSqlConfig().getDialect().getEscapeChar());

		// 自動パラメータバインド関数の呼出
		if (sqlContext.batchCount() == 0) {
			if (isQuery) {
				sqlContext.acceptQueryAutoParameterBinder();
			} else {
				sqlContext.acceptUpdateAutoParameterBinder();
			}
		}

		if (StringUtils.isEmpty(sqlContext.getExecutableSql())) {
			boolean outputBindComment = (boolean) sqlContext.contextAttrs().getOrDefault(
					CTX_ATTR_KEY_OUTPUT_BIND_COMMENT, true);
			SqlParser sqlParser = new SqlParserImpl(originalSql, sqlConfig.getDialect().isRemoveTerminator(),
					outputBindComment);
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
		LOG.debug("Executed SQL[{}{}{}]", System.lineSeparator(), sqlContext.getExecutableSql(),
				System.lineSeparator());
	}

	/**
	 * フェッチサイズとクエリタイムアウトをPreparedStatementに設定する
	 *
	 * @param preparedStatement PreparedStatement
	 * @throws SQLException SQL例外
	 */
	protected void applyProperties(final PreparedStatement preparedStatement) throws SQLException {
		// フェッチサイズ指定
		if (getFetchSize() >= 0 && !(preparedStatement instanceof CallableStatement)) {
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
	public void close() {
		transactionManager.close();
		if (coverageHandlerRef.get() != null) {
			coverageHandlerRef.get().onSqlAgentClose();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void required(final SQLRunnable runnable) {
		transactionManager.required(runnable);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R required(final SQLSupplier<R> supplier) {
		return transactionManager.required(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void requiresNew(final SQLRunnable runnable) {
		transactionManager.requiresNew(runnable);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R requiresNew(final SQLSupplier<R> supplier) {
		return transactionManager.requiresNew(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void notSupported(final SQLRunnable runnable) {
		transactionManager.notSupported(runnable);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R notSupported(final SQLSupplier<R> supplier) {
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
	public void setSavepoint(final String savepointName) {
		transactionManager.setSavepoint(savepointName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#releaseSavepoint(java.lang.String)
	 */
	@Override
	public void releaseSavepoint(final String savepointName) {
		transactionManager.releaseSavepoint(savepointName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#rollback(java.lang.String)
	 */
	@Override
	public void rollback(final String savepointName) {
		transactionManager.rollback(savepointName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#rollback()
	 */
	@Override
	public void rollback() {
		transactionManager.rollback();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#commit()
	 */
	@Override
	public void commit() {
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
		return new SqlQueryImpl(this, contextFrom(sqlName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#queryWith(java.lang.String)
	 */
	@Override
	public SqlQuery queryWith(final String sql) {
		return new SqlQueryImpl(this, contextWith(sql));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#update(java.lang.String)
	 */
	@Override
	public SqlUpdate update(final String sqlName) {
		return new SqlUpdateImpl(this, contextFrom(sqlName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updateWith(java.lang.String)
	 */
	@Override
	public SqlUpdate updateWith(final String sql) {
		return new SqlUpdateImpl(this, contextWith(sql));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#batch(java.lang.String)
	 */
	@Override
	public SqlBatch batch(final String sqlName) {
		return new SqlBatchImpl(this, contextFrom(sqlName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#batchWith(java.lang.String)
	 */
	@Override
	public SqlBatch batchWith(final String sql) {
		return new SqlBatchImpl(this, contextWith(sql));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#proc(java.lang.String)
	 */
	@Override
	public Procedure proc(final String sqlName) {
		return new ProcedureImpl(this, contextFrom(sqlName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#procWith(java.lang.String)
	 */
	@Override
	public Procedure procWith(final String sql) {
		return new ProcedureImpl(this, contextWith(sql));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(java.lang.Class)
	 */
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

			SqlContext context = handler.createSelectContext(this, metadata, entityType, false);

			return new SqlEntityQueryImpl<>(this, handler, metadata, context, entityType);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.SELECT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> int inserts(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition,
			final InsertsType insertsType) {
		if (insertsType == InsertsType.BULK && sqlConfig.getDialect().supportsBulkInsert()) {
			return bulkInsert(entityType, entities, condition, null);
		} else {
			return batchInsert(entityType, entities, condition, null);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition)
	 */
	@Override
	public <E> int inserts(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition) {
		return inserts(entityType, entities, condition, defaultInsertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.lang.Class, java.util.stream.Stream)
	 */
	@Override
	public <E> int inserts(final Class<E> entityType, final Stream<E> entities) {
		return inserts(entityType, entities, DEFAULT_INSERTS_WHEN_CONDITION);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> int inserts(final Class<E> entityType, final Stream<E> entities, final InsertsType insertsType) {
		return inserts(entityType, entities, DEFAULT_INSERTS_WHEN_CONDITION, insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> int inserts(final Stream<E> entities, final InsertsCondition<? super E> condition,
			final InsertsType insertsType) {
		Iterator<E> iterator = entities.iterator();
		if (!iterator.hasNext()) {
			return 0;
		}

		E firstEntity = iterator.next();

		Spliterator<E> spliterator = Spliterators.spliteratorUnknownSize(iterator, Spliterator.NONNULL);
		Stream<E> otherStream = StreamSupport.stream(spliterator, false);
		Stream<E> stream = Stream.concat(Stream.of(firstEntity), otherStream);

		@SuppressWarnings("unchecked")
		Class<E> type = (Class<E>) firstEntity.getClass();

		return inserts(type, stream, condition, insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition)
	 */
	@Override
	public <E> int inserts(final Stream<E> entities, final InsertsCondition<? super E> condition) {
		return inserts(entities, condition, defaultInsertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.util.stream.Stream)
	 */
	@Override
	public <E> int inserts(final Stream<E> entities) {
		return inserts(entities, DEFAULT_INSERTS_WHEN_CONDITION);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.util.stream.Stream, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> int inserts(final Stream<E> entities, final InsertsType insertsType) {
		return inserts(entities, DEFAULT_INSERTS_WHEN_CONDITION, insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition,
			final InsertsType insertsType) {
		List<E> insertedEntities = new ArrayList<>();
		if (insertsType == InsertsType.BULK && sqlConfig.getDialect().supportsBulkInsert()) {
			bulkInsert(entityType, entities, condition, insertedEntities);
		} else {
			batchInsert(entityType, entities, condition, insertedEntities);
		}
		return insertedEntities.stream();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition) {
		return insertsAndReturn(entityType, entities, condition, defaultInsertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.lang.Class, java.util.stream.Stream)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Class<E> entityType, final Stream<E> entities) {
		return insertsAndReturn(entityType, entities, DEFAULT_INSERTS_WHEN_CONDITION);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Class<E> entityType, final Stream<E> entities,
			final InsertsType insertsType) {
		return insertsAndReturn(entityType, entities, DEFAULT_INSERTS_WHEN_CONDITION, insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Stream<E> entities, final InsertsCondition<? super E> condition,
			final InsertsType insertsType) {
		Iterator<E> iterator = entities.iterator();
		if (!iterator.hasNext()) {
			return new ArrayList<E>().stream();
		}

		E firstEntity = iterator.next();

		Spliterator<E> spliterator = Spliterators.spliteratorUnknownSize(iterator, Spliterator.NONNULL);
		Stream<E> otherStream = StreamSupport.stream(spliterator, false);
		Stream<E> stream = Stream.concat(Stream.of(firstEntity), otherStream);

		@SuppressWarnings("unchecked")
		Class<E> type = (Class<E>) firstEntity.getClass();

		return insertsAndReturn(type, stream, condition, insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Stream<E> entities, final InsertsCondition<? super E> condition) {
		return insertsAndReturn(entities, condition, defaultInsertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.util.stream.Stream)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Stream<E> entities) {
		return insertsAndReturn(entities, DEFAULT_INSERTS_WHEN_CONDITION);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.util.stream.Stream, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Stream<E> entities, final InsertsType insertsType) {
		return insertsAndReturn(entities, DEFAULT_INSERTS_WHEN_CONDITION, insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updates(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.UpdatesCondition)
	 */
	@Override
	public <E> int updates(final Class<E> entityType, final Stream<E> entities,
			final UpdatesCondition<? super E> condition) {
		return batchUpdate(entityType, entities, condition, null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updatesAndReturn(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.UpdatesCondition)
	 */
	@Override
	public <E> Stream<E> updatesAndReturn(final Class<E> entityType, final Stream<E> entities,
			final UpdatesCondition<? super E> condition) {
		List<E> updatedEntites = new ArrayList<>();
		batchUpdate(entityType, entities, condition, updatedEntites);
		return updatedEntites.stream();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updates(java.lang.Class, java.util.stream.Stream)
	 */
	@Override
	public <E> int updates(final Class<E> entityType, final Stream<E> entities) {
		return updates(entityType, entities, DEFAULT_UPDATES_WHEN_CONDITION);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updatesAndReturn(java.lang.Class, java.util.stream.Stream)
	 */
	@Override
	public <E> Stream<E> updatesAndReturn(final Class<E> entityType, final Stream<E> entities) {
		return updatesAndReturn(entityType, entities, DEFAULT_UPDATES_WHEN_CONDITION);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updates(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.UpdatesCondition)
	 */
	@Override
	public <E> int updates(final Stream<E> entities, final UpdatesCondition<? super E> condition) {
		Iterator<E> iterator = entities.iterator();
		if (!iterator.hasNext()) {
			return 0;
		}

		E firstEntity = iterator.next();

		Spliterator<E> spliterator = Spliterators.spliteratorUnknownSize(iterator, Spliterator.NONNULL);
		Stream<E> otherStream = StreamSupport.stream(spliterator, false);
		Stream<E> stream = Stream.concat(Stream.of(firstEntity), otherStream);

		@SuppressWarnings("unchecked")
		Class<E> type = (Class<E>) firstEntity.getClass();

		return updates(type, stream, condition);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updatesAndReturn(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.UpdatesCondition)
	 */
	@Override
	public <E> Stream<E> updatesAndReturn(final Stream<E> entities, final UpdatesCondition<? super E> condition) {
		Iterator<E> iterator = entities.iterator();
		if (!iterator.hasNext()) {
			return new ArrayList<E>().stream();
		}

		E firstEntity = iterator.next();

		Spliterator<E> spliterator = Spliterators.spliteratorUnknownSize(iterator, Spliterator.NONNULL);
		Stream<E> otherStream = StreamSupport.stream(spliterator, false);
		Stream<E> stream = Stream.concat(Stream.of(firstEntity), otherStream);

		@SuppressWarnings("unchecked")
		Class<E> type = (Class<E>) firstEntity.getClass();

		return updatesAndReturn(type, stream, condition);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updates(java.util.stream.Stream)
	 */
	@Override
	public <E> int updates(final Stream<E> entities) {
		return updates(entities, DEFAULT_UPDATES_WHEN_CONDITION);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updatesAndReturn(java.util.stream.Stream)
	 */
	@Override
	public <E> Stream<E> updatesAndReturn(final Stream<E> entities) {
		return updatesAndReturn(entities, DEFAULT_UPDATES_WHEN_CONDITION);
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
	 * Queryの結果を格納するMapのキーを生成する際に使用するCaseFormatを取得する
	 *
	 * @return Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat
	 */
	@Override
	public CaseFormat getDefaultMapKeyCaseFormat() {
		return defaultMapKeyCaseFormat;
	}

	/**
	 * Queryの結果を格納するMapのキーを生成する際に使用するCaseFormatを設定する。
	 *
	 * @param defaultMapKeyCaseFormat Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat
	 */
	@Override
	public void setDefaultMapKeyCaseFormat(final CaseFormat defaultMapKeyCaseFormat) {
		this.defaultMapKeyCaseFormat = defaultMapKeyCaseFormat;
	}

	/**
	 * デフォルトの{@link InsertsType}を取得する
	 *
	 * @return insertsType
	 * @see jp.co.future.uroborosql.enums.InsertsType
	 */
	@Override
	public InsertsType getDefaultInsertsType() {
		return defaultInsertsType;
	}

	/**
	 * デフォルトの{@link InsertsType}を設定する
	 *
	 * @param defaultInsertsType デフォルトの{@link InsertsType}
	 * @see jp.co.future.uroborosql.enums.InsertsType
	 */
	@Override
	public void setDefaultInsertsType(final InsertsType defaultInsertsType) {
		this.defaultInsertsType = defaultInsertsType;
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

	/**
	 * 複数エンティティのBULK INSERTを実行
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括INSERT用のフレームの判定条件
	 * @param insertedEntities INSERTしたEntityのList. <code>null</code>の場合は格納されない
	 * @return SQL実行結果
	 */
	protected abstract <E> int batchInsert(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition, List<E> insertedEntities);

	/**
	 * 複数エンティティのINSERTをバッチ実行
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括INSERT用のフレームの判定条件
	 * @param insertedEntities INSERTしたEntityのList. <code>null</code>の場合は格納されない
	 * @return SQL実行結果
	 */
	protected abstract <E> int bulkInsert(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition, List<E> insertedEntities);

	/**
	 * 複数エンティティのBULK UPDATEを実行
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括更新用のフレームの判定条件
	 * @param updatedEntities INSERTしたEntityのList. <code>null</code>の場合は格納されない
	 * @return SQL実行結果
	 */
	protected abstract <E> int batchUpdate(final Class<E> entityType, final Stream<E> entities,
			final UpdatesCondition<? super E> condition, List<E> updatedEntities);

}
