/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import jp.co.future.uroborosql.client.SqlParamUtils;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.context.ExecutionContextImpl;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.coverage.CoverageData;
import jp.co.future.uroborosql.coverage.CoverageHandler;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.event.BeforeParseSqlEvent;
import jp.co.future.uroborosql.event.ProcedureEvent;
import jp.co.future.uroborosql.event.SqlBatchEvent;
import jp.co.future.uroborosql.event.SqlQueryEvent;
import jp.co.future.uroborosql.event.SqlUpdateEvent;
import jp.co.future.uroborosql.event.TransformSqlEvent;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.exception.OptimisticLockException;
import jp.co.future.uroborosql.exception.PessimisticLockException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.fluent.SqlBatch;
import jp.co.future.uroborosql.fluent.SqlEntityDelete;
import jp.co.future.uroborosql.fluent.SqlEntityQuery;
import jp.co.future.uroborosql.fluent.SqlEntityUpdate;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.fluent.SqlUpdate;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.MappingColumn;
import jp.co.future.uroborosql.mapping.MappingUtils;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parser.SqlParserImpl;
import jp.co.future.uroborosql.store.SqlResourceManager;
import jp.co.future.uroborosql.tx.LocalTransactionManager;
import jp.co.future.uroborosql.tx.TransactionManager;
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * SQL実行用クラス。
 *
 * @author H.Sugimoto
 */
public class SqlAgentImpl implements SqlAgent {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.log");

	/** SQLロガー */
	private static final Logger SQL_LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.sql");

	/** SQLカバレッジ用ロガー */
	private static final Logger COVERAGE_LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.sql.coverage");

	/** パフォーマンスロガー */
	private static final Logger PERFORMANCE_LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.performance");

	/** REPLロガー */
	private static final Logger REPL_LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.repl");

	/** ログ出力を抑止するためのMDCキー */
	private static final String SUPPRESS_PARAMETER_LOG_OUTPUT = "SuppressParameterLogOutput";

	/** ExecutionContext属性キー:リトライカウント */
	private static final String CTX_ATTR_KEY_RETRY_COUNT = "__retryCount";

	/** ExecutionContext属性キー:バインドパラメータコメントの出力有無 */
	private static final String CTX_ATTR_KEY_OUTPUT_BIND_COMMENT = "__outputBindComment";

	/** 例外発生にロールバックが必要なDBでリトライを実現するために設定するSavepointの名前 */
	private static final String RETRY_SAVEPOINT_NAME = "__retry_savepoint";

	/** IN句に渡すパラメータのMAXサイズ */
	private static final int IN_CLAUSE_MAX_PARAM_SIZE = 1000;

	/** 経過時間のフォーマッタ */
	private static final DateTimeFormatter ELAPSED_TIME_FORMAT = DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS");

	/** カバレッジハンドラ */
	private static final AtomicReference<CoverageHandler> coverageHandlerRef = new AtomicReference<>();

	/** SQL設定管理クラス */
	private final SqlConfig sqlConfig;

	/** トランザクション管理機能 */
	private final TransactionManager transactionManager;

	/** 例外発生時のログ出力を行うかどうか デフォルトは<code>true</code> */
	private final boolean outputExceptionLog;

	/** クエリータイムアウト制限値 */
	private int queryTimeout = -1;

	/** フェッチサイズ */
	private int fetchSize = -1;

	/** SQL実行エラー時にリトライするエラーコードのリスト */
	private List<String> sqlRetryCodes = List.of();

	/** SQL実行エラー時の最大リトライ回数 */
	private int maxRetryCount = 0;

	/** SQL実行リトライ時の待機時間(ms) */
	private int retryWaitTime = 0;

	/** SQLを特定するための一意なIDに置換するためのキー */
	private final String keySqlId;

	/** Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat */
	private CaseFormat mapKeyCaseFormat = CaseFormat.UPPER_SNAKE_CASE;

	/** {@link InsertsType} */
	private InsertsType insertsType = InsertsType.BATCH;

	static {
		// SQLカバレッジ取得用のクラス名を設定する。指定がない場合、またはfalseが指定された場合はカバレッジを収集しない。
		// クラス名が指定されている場合はそのクラス名を指定
		var sqlCoverageClassName = System.getProperty(KEY_SQL_COVERAGE);
		if (sqlCoverageClassName == null) {
			COVERAGE_LOG.info("system property - uroborosql.sql.coverage not set. sql coverage turned off.");
		} else if (Boolean.FALSE.toString().equalsIgnoreCase(sqlCoverageClassName)) {
			sqlCoverageClassName = null;
			COVERAGE_LOG.info("system property - uroborosql.sql.coverage is set to false. sql coverage turned off.");
		} else if (Boolean.TRUE.toString().equalsIgnoreCase(sqlCoverageClassName)) {
			// trueの場合は、デフォルト値を設定
			sqlCoverageClassName = "jp.co.future.uroborosql.coverage.CoberturaCoverageHandler";
			COVERAGE_LOG.info("system property - uroborosql.sql.coverage is set to true. sql coverage turned on.");
		}

		CoverageHandler handler = null;
		if (sqlCoverageClassName != null) {
			try {
				handler = (CoverageHandler) Class.forName(sqlCoverageClassName, true,
						Thread.currentThread().getContextClassLoader()).getConstructor().newInstance();
				COVERAGE_LOG.info("CoverageHandler : {}", sqlCoverageClassName);
			} catch (Exception ex) {
				COVERAGE_LOG.warn("Failed to instantiate CoverageHandler class. Class:{}, Cause:{}",
						sqlCoverageClassName,
						ex.getMessage());
				COVERAGE_LOG.info("Turn off sql coverage due to failure to instantiate CoverageHandler class.");
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
	 * @param settings 設定情報
	 * @param connectionContext DB接続情報
	 */
	SqlAgentImpl(final SqlConfig sqlConfig, final Map<String, String> settings,
			final ConnectionContext connectionContext) {
		this.sqlConfig = sqlConfig;
		this.transactionManager = new LocalTransactionManager(sqlConfig, connectionContext);

		// デフォルトプロパティ設定
		if (settings.containsKey(SqlAgentProvider.PROPS_KEY_FETCH_SIZE)) {
			this.fetchSize = Integer.parseInt(settings.get(SqlAgentProvider.PROPS_KEY_FETCH_SIZE));
		}
		if (settings.containsKey(SqlAgentProvider.PROPS_KEY_QUERY_TIMEOUT)) {
			this.queryTimeout = Integer.parseInt(settings.get(SqlAgentProvider.PROPS_KEY_QUERY_TIMEOUT));
		}
		if (settings.containsKey(SqlAgentProvider.PROPS_KEY_SQL_RETRY_CODES)) {
			this.sqlRetryCodes = Collections.unmodifiableList(List.of(settings.get(
					SqlAgentProvider.PROPS_KEY_SQL_RETRY_CODES).split(",")));
		}
		if (settings.containsKey(SqlAgentProvider.PROPS_KEY_DEFAULT_MAX_RETRY_COUNT)) {
			this.maxRetryCount = Integer.parseInt(settings.get(SqlAgentProvider.PROPS_KEY_DEFAULT_MAX_RETRY_COUNT));
		}
		if (settings.containsKey(SqlAgentProvider.PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME)) {
			this.retryWaitTime = Integer.parseInt(settings
					.get(SqlAgentProvider.PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME));
		}
		if (settings.containsKey(SqlAgentProvider.PROPS_KEY_SQL_ID_KEY_NAME)) {
			this.keySqlId = settings.get(SqlAgentProvider.PROPS_KEY_SQL_ID_KEY_NAME);
		} else {
			this.keySqlId = "_SQL_ID_";
		}
		if (settings.containsKey(SqlAgentProvider.PROPS_KEY_DEFAULT_MAP_KEY_CASE_FORMAT)) {
			this.mapKeyCaseFormat = CaseFormat.valueOf(settings
					.get(SqlAgentProvider.PROPS_KEY_DEFAULT_MAP_KEY_CASE_FORMAT));
		}
		if (settings.containsKey(SqlAgentProvider.PROPS_KEY_DEFAULT_INSERTS_TYPE)) {
			this.insertsType = InsertsType.valueOf(settings
					.get(SqlAgentProvider.PROPS_KEY_DEFAULT_INSERTS_TYPE));
		}
		if (settings.containsKey(SqlAgentProviderImpl.PROPS_KEY_OUTPUT_EXCEPTION_LOG)) {
			outputExceptionLog = Boolean
					.parseBoolean(settings.get(SqlAgentProviderImpl.PROPS_KEY_OUTPUT_EXCEPTION_LOG));
		} else {
			outputExceptionLog = true;
		}
	}

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
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#getConnection()
	 */
	@Override
	public Connection getConnection() {
		return transactionManager.getConnection();
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(java.lang.Runnable)
	 */
	@Override
	public void required(final Runnable runnable) {
		transactionManager.required(runnable);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(java.util.function.Supplier)
	 */
	@Override
	public <R> R required(final Supplier<R> supplier) {
		return transactionManager.required(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(jp.co.future.uroborosql.tx.Runnable)
	 */
	@Override
	public void requiresNew(final Runnable runnable) {
		transactionManager.requiresNew(runnable);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(java.util.function.Supplier)
	 */
	@Override
	public <R> R requiresNew(final Supplier<R> supplier) {
		return transactionManager.requiresNew(supplier);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(java.lang.Runnable)
	 */
	@Override
	public void notSupported(final Runnable runnable) {
		transactionManager.notSupported(runnable);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(java.util.function.Supplier)
	 */
	@Override
	public <R> R notSupported(final Supplier<R> supplier) {
		return transactionManager.notSupported(supplier);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#setRollbackOnly()
	 */
	@Override
	public void setRollbackOnly() {
		transactionManager.setRollbackOnly();
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#setSavepoint(java.lang.String)
	 */
	@Override
	public void setSavepoint(final String savepointName) {
		transactionManager.setSavepoint(savepointName);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#releaseSavepoint(java.lang.String)
	 */
	@Override
	public void releaseSavepoint(final String savepointName) {
		transactionManager.releaseSavepoint(savepointName);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#rollback(java.lang.String)
	 */
	@Override
	public void rollback(final String savepointName) {
		transactionManager.rollback(savepointName);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#savepointScope(java.util.function.Supplier)
	 */
	@Override
	public <R> R savepointScope(final Supplier<R> supplier) {
		return transactionManager.savepointScope(supplier);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#savepointScope(java.lang.Runnable)
	 */
	@Override
	public void savepointScope(final Runnable runnable) {
		transactionManager.savepointScope(runnable);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#autoCommitScope(java.util.function.Supplier)
	 */
	@Override
	public <R> R autoCommitScope(final Supplier<R> supplier) {
		return transactionManager.autoCommitScope(supplier);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#autoCommitScope(java.lang.Runnable)
	 */
	@Override
	public void autoCommitScope(final Runnable runnable) {
		transactionManager.autoCommitScope(runnable);
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#commit()
	 */
	@Override
	public void commit() {
		transactionManager.commit();
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#rollback()
	 */
	@Override
	public void rollback() {
		transactionManager.rollback();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(java.lang.String)
	 */
	@Override
	public SqlQuery query(final String sqlName) {
		return new SqlQueryImpl(this, context().setSqlName(sqlName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(java.util.function.Supplier)
	 */
	@Override
	public SqlQuery query(final Supplier<String> supplier) {
		if (supplier == null) {
			throw new IllegalArgumentException("supplier is required.");
		}
		return new SqlQueryImpl(this, context().setSqlName(supplier.get()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#queryWith(java.lang.String)
	 */
	@Override
	public SqlQuery queryWith(final String sql) {
		return new SqlQueryImpl(this, context().setSql(sql));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#update(java.lang.String)
	 */
	@Override
	public SqlUpdate update(final String sqlName) {
		return new SqlUpdateImpl(this, context().setSqlName(sqlName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#update(java.util.function.Supplier)
	 */
	@Override
	public SqlUpdate update(final Supplier<String> supplier) {
		if (supplier == null) {
			throw new IllegalArgumentException("supplier is required.");
		}
		return new SqlUpdateImpl(this, context().setSqlName(supplier.get()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updateWith(java.lang.String)
	 */
	@Override
	public SqlUpdate updateWith(final String sql) {
		return new SqlUpdateImpl(this, context().setSql(sql));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#batch(java.lang.String)
	 */
	@Override
	public SqlBatch batch(final String sqlName) {
		return new SqlBatchImpl(this, context().setSqlName(sqlName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#batch(java.util.function.Supplier)
	 */
	@Override
	public SqlBatch batch(final Supplier<String> supplier) {
		if (supplier == null) {
			throw new IllegalArgumentException("supplier is required.");
		}
		return new SqlBatchImpl(this, context().setSqlName(supplier.get()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#batchWith(java.lang.String)
	 */
	@Override
	public SqlBatch batchWith(final String sql) {
		return new SqlBatchImpl(this, context().setSql(sql));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#proc(java.lang.String)
	 */
	@Override
	public Procedure proc(final String sqlName) {
		return new ProcedureImpl(this, context().setSqlName(sqlName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#proc(java.util.function.Supplier)
	 */
	@Override
	public Procedure proc(final Supplier<String> supplier) {
		if (supplier == null) {
			throw new IllegalArgumentException("supplier is required.");
		}
		return new ProcedureImpl(this, context().setSqlName(supplier.get()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#procWith(java.lang.String)
	 */
	@Override
	public Procedure procWith(final String sql) {
		return new ProcedureImpl(this, context().setSql(sql));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(java.lang.Class)
	 */
	@Override
	public <E> SqlEntityQuery<E> query(final Class<? extends E> entityType) {
		var handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}
		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = handler.createSelectContext(this, metadata, entityType, false);
			context.setSqlKind(SqlKind.ENTITY_SELECT);
			return new SqlEntityQueryImpl<>(this, handler, metadata, context, entityType);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_SELECT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> int inserts(final Class<E> entityType,
			final Stream<E> entities,
			final InsertsCondition<? super E> condition,
			final InsertsType insertsType) {
		if (insertsType == InsertsType.BULK && getDialect().supportsBulkInsert()) {
			return bulkInsert(entityType, entities, condition, null);
		} else {
			return batchInsert(entityType, entities, condition, null);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.lang.Class, java.util.stream.Stream)
	 */
	@Override
	public <E> int inserts(final Class<E> entityType,
			final Stream<E> entities) {
		return inserts(entityType, entities, getInsertsCondition(getInsertsType()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition)
	 */
	@Override
	public <E> int inserts(final Class<E> entityType,
			final Stream<E> entities,
			final InsertsCondition<? super E> condition) {
		return inserts(entityType, entities, condition, getInsertsType());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> int inserts(final Class<E> entityType,
			final Stream<E> entities,
			final InsertsType insertsType) {
		return inserts(entityType, entities, getInsertsCondition(insertsType), insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> int inserts(final Stream<E> entities,
			final InsertsCondition<? super E> condition,
			final InsertsType insertsType) {
		var iterator = entities.iterator();
		if (!iterator.hasNext()) {
			return 0;
		}
		var firstEntity = iterator.next();
		@SuppressWarnings("unchecked")
		var type = (Class<E>) firstEntity.getClass();
		var stream = Stream.concat(Stream.of(firstEntity),
				StreamSupport.stream(Spliterators.spliteratorUnknownSize(iterator, Spliterator.NONNULL), false));
		return inserts(type, stream, condition, insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.util.stream.Stream)
	 */
	@Override
	public <E> int inserts(final Stream<E> entities) {
		return inserts(entities, getInsertsCondition(getInsertsType()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition)
	 */
	@Override
	public <E> int inserts(final Stream<E> entities,
			final InsertsCondition<? super E> condition) {
		return inserts(entities, condition, getInsertsType());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#inserts(java.util.stream.Stream, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> int inserts(final Stream<E> entities,
			final InsertsType insertsType) {
		return inserts(entities, getInsertsCondition(insertsType), insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Class<E> entityType,
			final Stream<E> entities,
			final InsertsCondition<? super E> condition,
			final InsertsType insertsType) {
		var insertedEntities = new ArrayList<E>();
		if (insertsType == InsertsType.BULK && getDialect().supportsBulkInsert()) {
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
	public <E> Stream<E> insertsAndReturn(final Class<E> entityType,
			final Stream<E> entities,
			final InsertsCondition<? super E> condition) {
		return insertsAndReturn(entityType, entities, condition, getInsertsType());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.lang.Class, java.util.stream.Stream)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Class<E> entityType,
			final Stream<E> entities) {
		return insertsAndReturn(entityType, entities, getInsertsCondition(getInsertsType()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Class<E> entityType,
			final Stream<E> entities,
			final InsertsType insertsType) {
		return insertsAndReturn(entityType, entities, getInsertsCondition(insertsType), insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Stream<E> entities,
			final InsertsCondition<? super E> condition,
			final InsertsType insertsType) {
		var iterator = entities.iterator();
		if (!iterator.hasNext()) {
			return Stream.empty();
		}
		var firstEntity = iterator.next();
		@SuppressWarnings("unchecked")
		var type = (Class<E>) firstEntity.getClass();
		var stream = Stream.concat(Stream.of(firstEntity),
				StreamSupport.stream(Spliterators.spliteratorUnknownSize(iterator, Spliterator.NONNULL), false));
		return insertsAndReturn(type, stream, condition, insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Stream<E> entities,
			final InsertsCondition<? super E> condition) {
		return insertsAndReturn(entities, condition, getInsertsType());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.util.stream.Stream)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Stream<E> entities) {
		return insertsAndReturn(entities, getInsertsCondition(getInsertsType()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertsAndReturn(java.util.stream.Stream, jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public <E> Stream<E> insertsAndReturn(final Stream<E> entities,
			final InsertsType insertsType) {
		return insertsAndReturn(entities, getInsertsCondition(insertsType), insertsType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updates(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.UpdatesCondition)
	 */
	@Override
	public <E> int updates(final Class<E> entityType,
			final Stream<E> entities,
			final UpdatesCondition<? super E> condition) {
		return batchUpdate(entityType, entities, condition, null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updatesAndReturn(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.UpdatesCondition)
	 */
	@Override
	public <E> Stream<E> updatesAndReturn(final Class<E> entityType,
			final Stream<E> entities,
			final UpdatesCondition<? super E> condition) {
		var updatedEntities = new ArrayList<E>();
		batchUpdate(entityType, entities, condition, updatedEntities);
		return updatedEntities.stream();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updates(java.lang.Class, java.util.stream.Stream)
	 */
	@Override
	public <E> int updates(final Class<E> entityType,
			final Stream<E> entities) {
		return updates(entityType, entities, DEFAULT_UPDATES_WHEN_CONDITION);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updatesAndReturn(java.lang.Class, java.util.stream.Stream)
	 */
	@Override
	public <E> Stream<E> updatesAndReturn(final Class<E> entityType,
			final Stream<E> entities) {
		return updatesAndReturn(entityType, entities, DEFAULT_UPDATES_WHEN_CONDITION);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updates(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.UpdatesCondition)
	 */
	@Override
	public <E> int updates(final Stream<E> entities,
			final UpdatesCondition<? super E> condition) {
		var iterator = entities.iterator();
		if (!iterator.hasNext()) {
			return 0;
		}
		var firstEntity = iterator.next();
		@SuppressWarnings("unchecked")
		var type = (Class<E>) firstEntity.getClass();
		var stream = Stream.concat(Stream.of(firstEntity),
				StreamSupport.stream(Spliterators.spliteratorUnknownSize(iterator, Spliterator.NONNULL), false));
		return updates(type, stream, condition);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updatesAndReturn(java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.UpdatesCondition)
	 */
	@Override
	public <E> Stream<E> updatesAndReturn(final Stream<E> entities,
			final UpdatesCondition<? super E> condition) {
		var iterator = entities.iterator();
		if (!iterator.hasNext()) {
			return Stream.empty();
		}
		var firstEntity = iterator.next();
		@SuppressWarnings("unchecked")
		var type = (Class<E>) firstEntity.getClass();
		var stream = Stream.concat(Stream.of(firstEntity),
				StreamSupport.stream(Spliterators.spliteratorUnknownSize(iterator, Spliterator.NONNULL), false));
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

	private <E> InsertsCondition<? super E> getInsertsCondition(final InsertsType insertsType) {
		return InsertsType.BATCH.equals(insertsType)
				? DEFAULT_BATCH_INSERTS_WHEN_CONDITION
				: DEFAULT_BULK_INSERTS_WHEN_CONDITION;
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
	private <E> int batchInsert(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition, final List<E> insertedEntities) {
		EntityHandler<E> handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = handler.createBatchInsertContext(this, metadata, entityType);
			context.setSqlKind(SqlKind.ENTITY_BATCH_INSERT);

			var count = 0;
			var entityList = new ArrayList<E>();
			var isFirst = true;
			List<MappingColumn> autoGeneratedColumns = List.of();
			Map<String, Object> nonNullObjectIdFlags = null;
			for (var iterator = entities.iterator(); iterator.hasNext();) {
				var entity = iterator.next();

				if (!entityType.isInstance(entity)) {
					throw new IllegalArgumentException("Entity types do not match");
				}

				if (isFirst) {
					isFirst = false;
					var mappingColumns = MappingUtils.getMappingColumns(metadata.getSchema(), entityType);
					autoGeneratedColumns = getAutoGeneratedColumns(context, mappingColumns, metadata, entity);

					// SQLのID項目IF分岐判定をtrueにするために値が設定されているID項目を保持しておく
					var excludeColumns = autoGeneratedColumns;
					nonNullObjectIdFlags = Arrays.stream(mappingColumns)
							.filter(col -> !excludeColumns.contains(col)
									&& !col.getJavaType().getRawType().isPrimitive()
									&& col.getValue(entity) != null)
							.collect(Collectors.toMap(MappingColumn::getCamelName, col -> true));
				}

				entityList.add(entity);
				if (insertedEntities != null) {
					insertedEntities.add(entity);
				}

				handler.setInsertParams(context, entity);
				context.addBatch();
				// SQLのID項目IF分岐判定をtrueにするためにaddBatch()の後に保持しておいたID項目をcontextにバインドする
				if (nonNullObjectIdFlags != null && !nonNullObjectIdFlags.isEmpty()) {
					context.paramMap(nonNullObjectIdFlags);
				}
				if (condition.test(context, context.batchCount(), entity)) {
					count += Arrays.stream(doBatchInsert(context, handler, entityList, autoGeneratedColumns)).sum();
					entityList.clear();
				}
			}
			return count + (context.batchCount() != 0
					? Arrays.stream(doBatchInsert(context, handler, entityList, autoGeneratedColumns)).sum()
					: 0);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_BATCH_INSERT, e);
		}
	}

	private <E> int[] doBatchInsert(final ExecutionContext context, final EntityHandler<E> handler,
			final List<E> entityList, final List<MappingColumn> autoGeneratedColumns) throws SQLException {
		var counts = handler.doBatchInsert(this, context);
		if (!autoGeneratedColumns.isEmpty()) {
			var ids = context.getGeneratedKeyValues();
			var idx = 0;
			for (E ent : entityList) {
				for (var col : autoGeneratedColumns) {
					setEntityIdValue(ent, ids[idx++], col);
				}
			}
		}

		return counts;
	}

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
	private <E> int bulkInsert(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition, final List<E> insertedEntities) {
		EntityHandler<E> handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = handler.createBulkInsertContext(this, metadata, entityType);
			context.setSqlKind(SqlKind.ENTITY_BULK_INSERT);

			var frameCount = 0;
			var count = 0;
			var isFirst = true;
			List<MappingColumn> autoGeneratedColumns = List.of();
			Map<String, Object> nonNullObjectIdFlags = null;
			var entityList = new ArrayList<E>();
			for (var iterator = entities.iterator(); iterator.hasNext();) {
				var entity = iterator.next();

				if (!entityType.isInstance(entity)) {
					throw new IllegalArgumentException("Entity types do not match");
				}

				if (isFirst) {
					isFirst = false;
					var mappingColumns = MappingUtils.getMappingColumns(metadata.getSchema(), entityType);
					autoGeneratedColumns = getAutoGeneratedColumns(context, mappingColumns, metadata, entity);

					// SQLのID項目IF分岐判定をtrueにするために値が設定されているID項目を保持しておく
					var excludeColumns = autoGeneratedColumns;
					// indexなしのid値がcontextにバインドされないため、値としてkey=trueを退避しておく
					nonNullObjectIdFlags = Arrays.stream(mappingColumns)
							.filter(col -> !excludeColumns.contains(col)
									&& !col.getJavaType().getRawType().isPrimitive()
									&& col.getValue(entity) != null)
							.collect(Collectors.toMap(MappingColumn::getCamelName, col -> true));
				}
				// 退避しておいたid値をこのタイミングで設定する
				if (nonNullObjectIdFlags != null && !nonNullObjectIdFlags.isEmpty()) {
					context.paramMap(nonNullObjectIdFlags);
				}

				entityList.add(entity);
				if (insertedEntities != null) {
					insertedEntities.add(entity);
				}

				handler.setBulkInsertParams(context, entity, frameCount);
				frameCount++;

				if (condition.test(context, frameCount, entity)) {
					count += doBulkInsert(context, entityType, handler, metadata, autoGeneratedColumns, entityList);
					frameCount = 0;
					entityList.clear();

					// 新しいExecutionContextを作成する前にgeneratedKeyColumnsを退避しておく
					var generatedKeyColumns = context.getGeneratedKeyColumns();
					context = handler.createBulkInsertContext(this, metadata, entityType);
					context.setSqlKind(SqlKind.ENTITY_BULK_INSERT);
					// 実行結果から生成されたIDを取得できるようにPreparedStatementにIDカラムを渡す
					context.setGeneratedKeyColumns(generatedKeyColumns);
				}
			}
			return count + (frameCount > 0
					? doBulkInsert(context, entityType, handler, metadata, autoGeneratedColumns, entityList)
					: 0);

		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_BULK_INSERT, e);
		}
	}

	private <E> int doBulkInsert(final ExecutionContext context, final Class<E> entityType,
			final EntityHandler<E> handler,
			final TableMetadata metadata, final List<MappingColumn> autoGeneratedColumns, final List<E> entityList)
			throws SQLException {
		var count = handler.doBulkInsert(this,
				handler.setupSqlBulkInsertContext(this, context, metadata, entityType, entityList.size()));

		if (!autoGeneratedColumns.isEmpty()) {
			var ids = context.getGeneratedKeyValues();
			var idx = 0;
			for (E ent : entityList) {
				for (var col : autoGeneratedColumns) {
					setEntityIdValue(ent, ids[idx++], col);
				}
			}
		}

		return count;
	}

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
	private <E> int batchUpdate(final Class<E> entityType, final Stream<E> entities,
			final UpdatesCondition<? super E> condition, final List<E> updatedEntities) {
		var handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = handler.createBatchUpdateContext(this, metadata, entityType);
			context.setSqlKind(SqlKind.BATCH_UPDATE);

			var versionColumn = MappingUtils.getVersionMappingColumn(metadata.getSchema(),
					entityType);
			var entityCount = 0;
			var updateCount = 0;
			var entityList = new ArrayList<E>();
			for (var iterator = entities.iterator(); iterator.hasNext();) {
				var entity = iterator.next();

				if (!entityType.isInstance(entity)) {
					throw new IllegalArgumentException("Entity types do not match");
				}

				entityList.add(entity);
				if (updatedEntities != null) {
					updatedEntities.add(entity);
				}
				entityCount++;

				handler.setUpdateParams(context, entity);
				context.addBatch();

				if (condition.test(context, context.batchCount(), entity)) {
					updateCount += Arrays.stream(handler.doBatchUpdate(this, context)).sum();
					entityList.clear();
				}
			}
			updateCount = updateCount + (context.batchCount() != 0
					? Arrays.stream(handler.doBatchUpdate(this, context)).sum()
					: 0);

			if (updatedEntities != null && versionColumn.isPresent()) {
				var vColumn = versionColumn.get();
				var keyColumns = metadata.getColumns().stream()
						.filter(TableMetadata.Column::isKey)
						.sorted(Comparator.comparingInt(TableMetadata.Column::getKeySeq))
						.map(c -> MappingUtils.getMappingColumnMap(metadata.getSchema(), entityType, SqlKind.NONE)
								.get(c.getCamelColumnName()))
						.collect(Collectors.toList());

				if (keyColumns.size() == 1) {
					// 単一キーの場合はIN句で更新した行を一括取得し@Versionのついたフィールドを更新する
					var keyColumn = keyColumns.get(0);
					var updatedEntityMap = updatedEntities.stream()
							.collect(Collectors.groupingBy(e -> keyColumn.getValue(e)));

					// updatedEntitiesのサイズが大きいとin句の上限にあたるため、1000件ずつに分割して検索する
					var keyList = new ArrayList<>(updatedEntityMap.keySet());
					var entitySize = updatedEntities.size();

					for (var start = 0; start < entitySize; start = start + IN_CLAUSE_MAX_PARAM_SIZE) {
						var end = Math.min(start + IN_CLAUSE_MAX_PARAM_SIZE, entitySize);
						var subList = keyList.subList(start, end);

						query(entityType).in(keyColumn.getCamelName(), subList).stream()
								.map(e -> {
									var updatedEntity = updatedEntityMap.get(keyColumn.getValue(e)).get(0);
									vColumn.setValue(updatedEntity, vColumn.getValue(e));
									return updatedEntity;
								}).count();
					}
				} else if (keyColumns.size() > 1) {
					// 複合キーの場合はIN句で一括取得できないため1件ずつ取得して@Versionのついたフィールドを更新する
					updatedEntities.stream()
							.map(updatedEntity -> {
								var keyValues = keyColumns.stream().map(k -> k.getValue(updatedEntity)).toArray();
								find(entityType, keyValues).ifPresent(e -> {
									vColumn.setValue(updatedEntity, vColumn.getValue(e));
								});
								return updatedEntity;
							}).count();
				}
			}
			if (versionColumn.isPresent() && getDialect().supportsEntityBulkUpdateOptimisticLock()
					&& updateCount != entityCount) {
				// バージョンカラムの指定があり、更新件数と更新対象Entityの件数が不一致の場合は楽観ロックエラーとする
				throw new OptimisticLockException(String.format(
						"An error occurred due to optimistic locking.%nExecuted SQL [%n%s]%nBatch Entity Count: %d, Update Count: %d.",
						context.getExecutableSql(), entityCount, updateCount));
			}
			return updateCount;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.BATCH_UPDATE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(jp.co.future.uroborosql.context.ExecutionContext)
	 */
	@Override
	public ResultSet query(final ExecutionContext executionContext) throws SQLException {
		// パラメータログを出力する
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, Boolean.FALSE.toString());

		if (SqlKind.NONE.equals(executionContext.getSqlKind())) {
			executionContext.setSqlKind(SqlKind.SELECT);
		}

		// コンテキスト変換
		transformContext(executionContext);

		var stmt = getPreparedStatement(executionContext);

		// INパラメータ設定
		executionContext.bindParams(stmt);

		// REPLで実行するための文字列をREPLログに出力する
		outputReplLog(executionContext);

		Instant startTime = null;
		if (LOG.isDebugEnabled()) {
			LOG.debug("Execute query sql. sqlName: {}", executionContext.getSqlName());
		}
		if (PERFORMANCE_LOG.isInfoEnabled()) {
			startTime = Instant.now(getSqlConfig().getClock());
		}

		try {
			// デフォルト最大リトライ回数を取得し、個別指定（ExecutionContextの値）があれば上書き
			var maxRetryCount = getMaxRetryCount();
			if (executionContext.getMaxRetryCount() >= 0) {
				maxRetryCount = executionContext.getMaxRetryCount();
			}

			// デフォルトリトライ待機時間を取得し、個別指定（ExecutionContextの値）があれば上書き
			var retryWaitTime = getRetryWaitTime();
			if (executionContext.getRetryWaitTime() > 0) {
				retryWaitTime = executionContext.getRetryWaitTime();
			}
			var loopCount = 0;
			var dialect = getDialect();
			ResultSet rs = null;
			try {
				do {
					try {
						if (maxRetryCount > 0 && dialect.isRollbackToSavepointBeforeRetry()) {
							setSavepoint(RETRY_SAVEPOINT_NAME);
						}
						rs = stmt.executeQuery();
						// Query実行後イベント発行
						if (getSqlConfig().getEventListenerHolder().hasSqlQueryListener()) {
							var eventObj = new SqlQueryEvent(executionContext, rs, stmt);
							for (var listener : getSqlConfig().getEventListenerHolder().getSqlQueryListeners()) {
								listener.accept(eventObj);
							}
							rs = eventObj.getResultSet();
						}
						stmt.closeOnCompletion();
						return new InnerResultSet(rs, stmt);
					} catch (SQLException ex) {
						if (maxRetryCount > 0 && dialect.isRollbackToSavepointBeforeRetry()) {
							rollback(RETRY_SAVEPOINT_NAME);
						}
						var errorCode = String.valueOf(ex.getErrorCode());
						var sqlState = ex.getSQLState();
						var pessimisticLockingErrorCodes = dialect.getPessimisticLockingErrorCodes();
						if (maxRetryCount > loopCount
								&& (getSqlRetryCodes().contains(errorCode) || getSqlRetryCodes().contains(sqlState))) {
							if (LOG.isDebugEnabled()) {
								LOG.debug(String.format(
										"Caught the error code to be retried.(%d times). Retry after %,3d ms.",
										loopCount + 1, retryWaitTime));
							}
							if (retryWaitTime > 0) {
								try {
									Thread.sleep(retryWaitTime);
								} catch (InterruptedException ie) {
									// do nothing
								}
							}
						} else {
							if (pessimisticLockingErrorCodes.contains(errorCode)
									|| pessimisticLockingErrorCodes.contains(sqlState)) {
								throw new PessimisticLockException(executionContext, ex);
							} else {
								throw ex;
							}
						}
					} finally {
						if (maxRetryCount > 0 && dialect.isRollbackToSavepointBeforeRetry()) {
							releaseSavepoint(RETRY_SAVEPOINT_NAME);
						}
						executionContext.contextAttrs().put(CTX_ATTR_KEY_RETRY_COUNT, loopCount);
					}
				} while (maxRetryCount > loopCount++);
			} catch (SQLException | RuntimeException e) {
				if (rs != null && !rs.isClosed()) {
					rs.close();
				}
				throw e;
			}
			return null;
		} catch (SQLException ex) {
			handleException(executionContext, ex);
			return null;
		} finally {
			// 後処理
			if (PERFORMANCE_LOG.isInfoEnabled() && startTime != null) {
				PERFORMANCE_LOG.info("SQL execution time [{}({})] : [{}]",
						generateSqlName(executionContext),
						executionContext.getSqlKind(),
						formatElapsedTime(startTime, Instant.now(getSqlConfig().getClock())));
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(jp.co.future.uroborosql.context.ExecutionContext,
	 *      jp.co.future.uroborosql.converter.ResultSetConverter)
	 */
	@Override
	public <T> Stream<T> query(final ExecutionContext executionContext, final ResultSetConverter<T> converter)
			throws SQLException {
		var rs = query(executionContext);
		return StreamSupport.stream(new ResultSetSpliterator<>(rs, converter), false).onClose(() -> {
			try {
				if (rs != null && !rs.isClosed()) {
					rs.close();
				}
			} catch (SQLException ex) {
				// do nothing
			}
		});
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(jp.co.future.uroborosql.context.ExecutionContext,
	 *      jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public List<Map<String, Object>> query(final ExecutionContext executionContext, final CaseFormat caseFormat)
			throws SQLException {
		try (var stream = query(executionContext, new MapResultSetConverter(getSqlConfig(), caseFormat))) {
			return stream.collect(Collectors.toList());
		}
	}

	/**
	 * @see jp.co.future.uroborosql.SqlAgent#update(jp.co.future.uroborosql.context.ExecutionContext)
	 */
	@Override
	public int update(final ExecutionContext executionContext) throws SQLException {
		// パラメータログを出力する
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, Boolean.FALSE.toString());

		if (SqlKind.NONE.equals(executionContext.getSqlKind())) {
			executionContext.setSqlKind(SqlKind.UPDATE);
		}

		// コンテキスト変換
		transformContext(executionContext);

		// 更新移譲処理の指定がある場合は移譲処理を実行し結果を返却
		if (executionContext.getUpdateDelegate() != null) {
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
			if (LOG.isInfoEnabled()) {
				LOG.info("Performs update delegate of update process.");
			}
			return executionContext.getUpdateDelegate().apply(executionContext);
		}

		Instant startTime = null;

		try (var stmt = getPreparedStatement(executionContext)) {

			// INパラメータ設定
			executionContext.bindParams(stmt);

			// REPLで実行するための文字列をREPLログに出力する
			outputReplLog(executionContext);

			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute update sql. sqlName: {}", executionContext.getSqlName());
			}

			if (PERFORMANCE_LOG.isInfoEnabled()) {
				startTime = Instant.now(getSqlConfig().getClock());
			}

			// デフォルト最大リトライ回数を取得し、個別指定（ExecutionContextの値）があれば上書き
			var maxRetryCount = getMaxRetryCount();
			if (executionContext.getMaxRetryCount() >= 0) {
				maxRetryCount = executionContext.getMaxRetryCount();
			}

			// デフォルトリトライ待機時間を取得し、個別指定（ExecutionContextの値）があれば上書き
			var retryWaitTime = getRetryWaitTime();
			if (executionContext.getRetryWaitTime() > 0) {
				retryWaitTime = executionContext.getRetryWaitTime();
			}
			var loopCount = 0;
			do {
				try {
					if (maxRetryCount > 0 && getDialect().isRollbackToSavepointBeforeRetry()) {
						setSavepoint(RETRY_SAVEPOINT_NAME);
					}
					var count = stmt.executeUpdate();
					// Update実行後イベント発行
					if (getSqlConfig().getEventListenerHolder().hasSqlUpdateListener()) {
						var eventObj = new SqlUpdateEvent(executionContext, count, stmt);
						for (var listener : getSqlConfig().getEventListenerHolder().getSqlUpdateListeners()) {
							listener.accept(eventObj);
						}
						count = eventObj.getCount();
					}
					if ((SqlKind.INSERT.equals(executionContext.getSqlKind()) ||
							SqlKind.ENTITY_INSERT.equals(executionContext.getSqlKind()) ||
							SqlKind.BULK_INSERT.equals(executionContext.getSqlKind()) ||
							SqlKind.ENTITY_BULK_INSERT.equals(executionContext.getSqlKind()))
							&& executionContext.hasGeneratedKeyColumns()) {
						try (var rs = stmt.getGeneratedKeys()) {
							var generatedKeyValues = new ArrayList<>();
							while (rs.next()) {
								for (var i = 1; i <= executionContext.getGeneratedKeyColumns().length; i++) {
									generatedKeyValues.add(rs.getObject(i));
								}
							}
							executionContext.setGeneratedKeyValues(
									generatedKeyValues.toArray(new Object[generatedKeyValues.size()]));
						}
					}
					return count;
				} catch (SQLException ex) {
					if (maxRetryCount > 0 && getDialect().isRollbackToSavepointBeforeRetry()) {
						rollback(RETRY_SAVEPOINT_NAME);
					}
					if (maxRetryCount > loopCount) {
						var errorCode = String.valueOf(ex.getErrorCode());
						var sqlState = ex.getSQLState();
						if (getSqlRetryCodes().contains(errorCode) || getSqlRetryCodes().contains(sqlState)) {
							if (LOG.isDebugEnabled()) {
								LOG.debug(String.format(
										"Caught the error code to be retried.(%d times). Retry after %,3d ms.",
										loopCount + 1, retryWaitTime));
							}
							if (retryWaitTime > 0) {
								try {
									Thread.sleep(retryWaitTime);
								} catch (InterruptedException ie) {
									// do nothing
								}
							}
						} else {
							throw ex;
						}
					} else {
						throw ex;
					}
				} finally {
					if (maxRetryCount > 0 && getDialect().isRollbackToSavepointBeforeRetry()) {
						releaseSavepoint(RETRY_SAVEPOINT_NAME);
					}
					executionContext.contextAttrs().put(CTX_ATTR_KEY_RETRY_COUNT, loopCount);
				}
			} while (maxRetryCount > loopCount++);
			return 0;
		} catch (SQLException ex) {
			handleException(executionContext, ex);
			return 0;
		} finally {
			if (PERFORMANCE_LOG.isInfoEnabled() && startTime != null) {
				PERFORMANCE_LOG.info("SQL execution time [{}({})] : [{}]",
						generateSqlName(executionContext),
						executionContext.getSqlKind(),
						formatElapsedTime(startTime, Instant.now(getSqlConfig().getClock())));
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
	}

	/**
	 * @see jp.co.future.uroborosql.SqlAgent#batch(jp.co.future.uroborosql.context.ExecutionContext)
	 */
	@Override
	public int[] batch(final ExecutionContext executionContext) throws SQLException {
		// バッチ処理の場合大量のログが出力されるため、パラメータログの出力を抑止する
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, Boolean.TRUE.toString());

		if (SqlKind.NONE.equals(executionContext.getSqlKind())) {
			executionContext.setSqlKind(SqlKind.BATCH_INSERT);
		}

		// コンテキスト変換
		transformContext(executionContext);

		// 更新移譲処理の指定がある場合は移譲処理を実行し結果を返却
		if (executionContext.getUpdateDelegate() != null) {
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
			if (LOG.isInfoEnabled()) {
				LOG.info("Performs update delegate of batch process.");
			}
			return new int[] { executionContext.getUpdateDelegate().apply(executionContext) };
		}

		Instant startTime = null;

		try (var stmt = getPreparedStatement(executionContext)) {

			// INパラメータ設定
			executionContext.bindBatchParams(stmt);

			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute batch sql. sqlName: {}", executionContext.getSqlName());
			}
			if (PERFORMANCE_LOG.isInfoEnabled()) {
				startTime = Instant.now(getSqlConfig().getClock());
			}

			// デフォルト最大リトライ回数を取得し、個別指定（ExecutionContextの値）があれば上書き
			var maxRetryCount = getMaxRetryCount();
			if (executionContext.getMaxRetryCount() >= 0) {
				maxRetryCount = executionContext.getMaxRetryCount();
			}

			// デフォルトリトライ待機時間を取得し、個別指定（ExecutionContextの値）があれば上書き
			var retryWaitTime = getRetryWaitTime();
			if (executionContext.getRetryWaitTime() > 0) {
				retryWaitTime = executionContext.getRetryWaitTime();
			}
			var loopCount = 0;
			do {
				try {
					if (maxRetryCount > 0 && getDialect().isRollbackToSavepointBeforeRetry()) {
						setSavepoint(RETRY_SAVEPOINT_NAME);
					}
					var counts = stmt.executeBatch();
					// Batch実行後イベント発行
					if (getSqlConfig().getEventListenerHolder().hasSqlBatchListener()) {
						var eventObj = new SqlBatchEvent(executionContext, counts, stmt);
						for (var listener : getSqlConfig().getEventListenerHolder().getSqlBatchListeners()) {
							listener.accept(eventObj);
						}
						counts = eventObj.getCounts();
					}
					if ((SqlKind.BATCH_INSERT.equals(executionContext.getSqlKind()) ||
							SqlKind.ENTITY_BATCH_INSERT.equals(executionContext.getSqlKind()))
							&& executionContext.hasGeneratedKeyColumns()) {
						try (var rs = stmt.getGeneratedKeys()) {
							var generatedKeyValues = new ArrayList<>();
							while (rs.next()) {
								for (var i = 1; i <= executionContext.getGeneratedKeyColumns().length; i++) {
									generatedKeyValues.add(rs.getObject(i));
								}
							}
							executionContext.setGeneratedKeyValues(
									generatedKeyValues.toArray(new Object[generatedKeyValues.size()]));
						}
					}
					return counts;
				} catch (SQLException ex) {
					if (maxRetryCount > 0 && getDialect().isRollbackToSavepointBeforeRetry()) {
						rollback(RETRY_SAVEPOINT_NAME);
					}
					if (maxRetryCount > loopCount) {
						var errorCode = String.valueOf(ex.getErrorCode());
						var sqlState = ex.getSQLState();
						if (getSqlRetryCodes().contains(errorCode) || getSqlRetryCodes().contains(sqlState)) {
							if (LOG.isDebugEnabled()) {
								LOG.debug(String.format(
										"Caught the error code to be retried.(%d times). Retry after %,3d ms.",
										loopCount + 1, retryWaitTime));
							}
							if (retryWaitTime > 0) {
								try {
									Thread.sleep(retryWaitTime);
								} catch (InterruptedException ie) {
									// do nothing
								}
							}
						} else {
							throw ex;
						}
					} else {
						throw ex;
					}
				} finally {
					if (maxRetryCount > 0 && getDialect().isRollbackToSavepointBeforeRetry()) {
						releaseSavepoint(RETRY_SAVEPOINT_NAME);
					}
					executionContext.clearBatch();
					executionContext.contextAttrs().put(CTX_ATTR_KEY_RETRY_COUNT, loopCount);
				}
			} while (maxRetryCount > loopCount++);
			return null;
		} catch (SQLException ex) {
			handleException(executionContext, ex);
			return null;
		} finally {
			// 後処理
			if (PERFORMANCE_LOG.isInfoEnabled() && startTime != null) {
				PERFORMANCE_LOG.info("SQL execution time [{}({})] : [{}]",
						generateSqlName(executionContext),
						executionContext.getSqlKind(),
						formatElapsedTime(startTime, Instant.now(getSqlConfig().getClock())));
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#procedure(jp.co.future.uroborosql.context.ExecutionContext)
	 */
	@Override
	public Map<String, Object> procedure(final ExecutionContext executionContext) throws SQLException {
		// パラメータログを出力する
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, Boolean.FALSE.toString());

		// procedureやfunctionの場合、SQL文法エラーになるためバインドパラメータコメントを出力しない
		executionContext.contextAttrs().put(CTX_ATTR_KEY_OUTPUT_BIND_COMMENT, false);

		if (SqlKind.NONE.equals(executionContext.getSqlKind())) {
			executionContext.setSqlKind(SqlKind.PROCEDURE);
		}

		// コンテキスト変換
		transformContext(executionContext);

		Instant startTime = null;

		try (var callableStatement = getCallableStatement(executionContext)) {

			// パラメータ設定
			executionContext.bindParams(callableStatement);

			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute stored procedure. sqlName: {}", executionContext.getSqlName());
			}
			if (PERFORMANCE_LOG.isInfoEnabled()) {
				startTime = Instant.now(getSqlConfig().getClock());
			}

			// デフォルト最大リトライ回数を取得し、個別指定（ExecutionContextの値）があれば上書き
			var maxRetryCount = getMaxRetryCount();
			if (executionContext.getMaxRetryCount() >= 0) {
				maxRetryCount = executionContext.getMaxRetryCount();
			}

			// デフォルトリトライ待機時間を取得し、個別指定（ExecutionContextの値）があれば上書き
			var retryWaitTime = getRetryWaitTime();
			if (executionContext.getRetryWaitTime() > 0) {
				retryWaitTime = executionContext.getRetryWaitTime();
			}
			var loopCount = 0;
			do {
				try {
					if (maxRetryCount > 0 && getDialect().isRollbackToSavepointBeforeRetry()) {
						setSavepoint(RETRY_SAVEPOINT_NAME);
					}
					var result = callableStatement.execute();
					// Procedure実行後イベント発行
					if (getSqlConfig().getEventListenerHolder().hasProcedureListener()) {
						var eventObj = new ProcedureEvent(executionContext, result, callableStatement);
						for (var listener : getSqlConfig().getEventListenerHolder().getProcedureListeners()) {
							listener.accept(eventObj);
						}
						result = eventObj.isResult();
					}
					break;
				} catch (SQLException ex) {
					if (maxRetryCount > 0 && getDialect().isRollbackToSavepointBeforeRetry()) {
						rollback(RETRY_SAVEPOINT_NAME);
					}
					if (maxRetryCount > loopCount) {
						var errorCode = String.valueOf(ex.getErrorCode());
						var sqlState = ex.getSQLState();
						if (getSqlRetryCodes().contains(errorCode) || getSqlRetryCodes().contains(sqlState)) {
							if (LOG.isDebugEnabled()) {
								LOG.debug(String.format(
										"Caught the error code to be retried.(%d times). Retry after %,3d ms.",
										loopCount + 1, retryWaitTime));
							}
							if (retryWaitTime > 0) {
								try {
									Thread.sleep(retryWaitTime);
								} catch (InterruptedException ie) {
									// do nothing
								}
							}
						} else {
							throw ex;
						}
					} else {
						throw ex;
					}
				} finally {
					if (maxRetryCount > 0 && getDialect().isRollbackToSavepointBeforeRetry()) {
						releaseSavepoint(RETRY_SAVEPOINT_NAME);
					}
					executionContext.contextAttrs().put(CTX_ATTR_KEY_RETRY_COUNT, loopCount);
				}
			} while (maxRetryCount > loopCount++);
			// 結果取得
			return executionContext.getOutParams(callableStatement);
		} catch (SQLException ex) {
			handleException(executionContext, ex);
		} finally {
			if (PERFORMANCE_LOG.isInfoEnabled() && startTime != null) {
				PERFORMANCE_LOG.info("Stored procedure execution time [{}({})] : [{}]",
						generateSqlName(executionContext),
						executionContext.getSqlKind(),
						formatElapsedTime(startTime, Instant.now(getSqlConfig().getClock())));
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
		return null;
	}

	/**
	 * ExecutionContextの設定内容を元にSQLを構築する
	 *
	 * @param executionContext ExecutionContext
	 */
	private void transformContext(final ExecutionContext executionContext) {
		var originalSql = executionContext.getSql();
		var sqlName = executionContext.getSqlName();
		if (StringUtils.isEmpty(originalSql) && getSqlResourceManager() != null) {
			originalSql = getSqlResourceManager().getSql(sqlName);
			if (StringUtils.isEmpty(originalSql)) {
				throw new UroborosqlRuntimeException("sql file:[" + sqlName + "] is not found.");
			}
		}

		// SQL-IDの付与
		if (originalSql.contains(keySqlId)) {
			var sqlId = executionContext.getSqlId();
			if (StringUtils.isEmpty(sqlId)) {
				sqlId = sqlName;
			}
			if (StringUtils.isEmpty(sqlId)) {
				sqlId = String.valueOf(originalSql.hashCode());
			}

			originalSql = originalSql.replace(keySqlId, sqlId);
		}

		if (SQL_LOG.isInfoEnabled() && sqlName != null) {
			if (executionContext.getSqlKind().isEntityType()) {
				SQL_LOG.info("EntityClass : {}", sqlName);
			} else if (getSqlResourceManager().existSql(sqlName)) {
				SQL_LOG.info("SQLPath : {}", getSqlResourceManager().getSqlPath(sqlName));
			}
		}

		// SQL変換前イベント発行
		if (getSqlConfig().getEventListenerHolder().hasTransformSqlListener()) {
			var eventObj = new TransformSqlEvent(executionContext, originalSql);
			getSqlConfig().getEventListenerHolder().getTransformSqlListeners()
					.forEach(listener -> listener.accept(eventObj));
			originalSql = eventObj.getSql();
		}
		executionContext.setSql(originalSql);

		// Dialectに合わせたエスケープキャラクタの設定
		executionContext.param(Dialect.PARAM_KEY_ESCAPE_CHAR, getDialect().getEscapeChar());

		// SQLパース前イベントの呼出
		if (executionContext.batchCount() == 0
				&& getSqlConfig().getEventListenerHolder().hasBeforeParseSqlListener()) {
			var eventObj = new BeforeParseSqlEvent(executionContext);
			getSqlConfig().getEventListenerHolder().getBeforeParseSqlListeners()
					.forEach(listener -> listener.accept(eventObj));
		}

		if (SQL_LOG.isDebugEnabled()) {
			SQL_LOG.debug("Template SQL[{}{}{}]", System.lineSeparator(), originalSql, System.lineSeparator());
		}

		if (StringUtils.isEmpty(executionContext.getExecutableSql())) {
			// SQLパーサーによるパース処理
			var outputBindComment = (boolean) executionContext.contextAttrs().getOrDefault(
					CTX_ATTR_KEY_OUTPUT_BIND_COMMENT, true);
			var sqlParser = new SqlParserImpl(originalSql, sqlConfig.getExpressionParser(),
					getDialect().isRemoveTerminator(), outputBindComment);
			var contextTransformer = sqlParser.parse();
			contextTransformer.transform(executionContext);

			if (coverageHandlerRef.get() != null) {
				// SQLカバレッジ用のログを出力する
				var coverageData = new CoverageData(sqlName, originalSql,
						contextTransformer.getPassedRoute());
				if (COVERAGE_LOG.isDebugEnabled()) {
					COVERAGE_LOG.debug("coverage data: {}", coverageData);
				}

				coverageHandlerRef.get().accept(coverageData);
			}
		}

		if (SQL_LOG.isInfoEnabled()) {
			SQL_LOG.info("Executed SQL[{}{}{}]", System.lineSeparator(), executionContext.getExecutableSql(),
					System.lineSeparator());
		}
	}

	/** 時間計測用のログに出力するSQL名を生成する.
	 *
	 * @param executionContext ExecutionContext
	 * @return SQL名. SQL名が取得できない場合はSQL_ID、または空文字を返却する
	 */
	private String generateSqlName(final ExecutionContext executionContext) {
		if (executionContext.getSqlName() != null) {
			return executionContext.getSqlName();
		} else {
			return Objects.toString(executionContext.getSqlId(), "");
		}
	}

	/**
	 * 例外発生時ハンドラー
	 *
	 * @param executionContext ExecutionContext
	 * @param ex SQL例外
	 * @throws SQLException SQL例外
	 */
	private void handleException(final ExecutionContext executionContext, final SQLException ex) throws SQLException {
		var cause = ex;

		while (cause.getNextException() != null) {
			cause = cause.getNextException();
		}

		if (outputExceptionLog && LOG.isErrorEnabled()) {
			var builder = new StringBuilder();
			builder.append(System.lineSeparator()).append("Exception occurred in SQL execution.")
					.append(System.lineSeparator());
			builder.append("Executed SQL[").append(executionContext.getExecutableSql()).append("]")
					.append(System.lineSeparator());
			if (executionContext instanceof ExecutionContextImpl) {
				var bindParameters = ((ExecutionContextImpl) executionContext).getBindParameters();
				for (var i = 0; i < bindParameters.length; i++) {
					var parameter = bindParameters[i];
					builder.append("Bind Parameter.[INDEX[").append(i + 1).append("], ").append(parameter.toString())
							.append("]").append(System.lineSeparator());
				}
			}
			LOG.error(builder.toString(), cause);
		}

		throw cause;
	}

	/**
	 * REPLでSQLを実行するためのコマンドをログとしてREPL_LOGに出力する.
	 *
	 * @param executionContext executionContext
	 */
	private void outputReplLog(final ExecutionContext executionContext) {
		if (!(REPL_LOG.isInfoEnabled() &&
				executionContext.getSqlName() != null &&
				(SqlKind.SELECT.equals(executionContext.getSqlKind()) ||
						SqlKind.UPDATE.equals(executionContext.getSqlKind())))) {
			// REPLログ出力対象でない場合は何もしない
			return;
		}

		var builder = new StringBuilder();

		if (SqlKind.SELECT.equals(executionContext.getSqlKind())) {
			builder.append("query ");
		} else {
			builder.append("update ");
		}

		builder.append(executionContext.getSqlName());

		var params = new ArrayList<Parameter>();
		for (var bindName : executionContext.getBindNames()) {
			params.add(executionContext.getParam(bindName));
		}
		if (!params.isEmpty()) {
			builder.append(" ");
			builder.append(SqlParamUtils.formatPrams(params));
		}
		REPL_LOG.info("REPL command: {}", builder.toString());
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#find(java.lang.Class, java.lang.Object[])
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <E> Optional<E> find(final Class<? extends E> entityType, final Object... keys) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var keyNames = metadata.getColumns().stream()
					.filter(TableMetadata.Column::isKey)
					.sorted(Comparator.comparingInt(TableMetadata.Column::getKeySeq))
					.map(TableMetadata.Column::getColumnName)
					.map(CaseFormat.CAMEL_CASE::convert)
					.toArray(String[]::new);

			if (keyNames.length != keys.length) {
				throw new IllegalArgumentException("Number of keys does not match");
			}
			var params = new HashMap<String, Object>();
			for (var i = 0; i < keys.length; i++) {
				params.put(keyNames[i], keys[i]);
			}

			var context = handler.createSelectContext(this, metadata, entityType, true);
			context.setSqlKind(SqlKind.ENTITY_SELECT).paramMap(params);

			try (var stream = handler.doSelect(this, context, entityType)) {
				return stream.findFirst();
			}
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_SELECT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insert(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <E> int insert(final E entity) {
		if (entity instanceof Stream) {
			throw new IllegalArgumentException("Stream type not supported.");
		}

		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isInstance(entity)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var entityType = entity.getClass();
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = handler.createInsertContext(this, metadata, entityType);
			context.setSqlKind(SqlKind.ENTITY_INSERT);

			// 自動採番カラムの取得とcontextへの設定を行う
			var mappingColumns = MappingUtils.getMappingColumns(metadata.getSchema(), entityType);
			var autoGeneratedColumns = getAutoGeneratedColumns(context, mappingColumns, metadata, entity);

			handler.setInsertParams(context, entity);
			var count = handler.doInsert(this, context, entity);

			if (!autoGeneratedColumns.isEmpty()) {
				var ids = context.getGeneratedKeyValues();
				var idx = 0;
				for (var col : autoGeneratedColumns) {
					setEntityIdValue(entity, ids[idx++], col);
				}
			}

			return count;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_INSERT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insertAndReturn(java.lang.Object)
	 */
	@Override
	public <E> E insertAndReturn(final E entity) {
		insert(entity);
		return entity;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#update(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <E> int update(final E entity) {
		if (entity instanceof Stream) {
			throw new IllegalArgumentException("Stream type not supported.");
		}

		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isInstance(entity)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var type = entity.getClass();
			var metadata = handler.getMetadata(this.transactionManager, type);
			var context = handler.createUpdateContext(this, metadata, type, true);
			context.setSqlKind(SqlKind.ENTITY_UPDATE);
			handler.setUpdateParams(context, entity);
			var count = handler.doUpdate(this, context, entity);

			MappingUtils.getVersionMappingColumn(metadata.getSchema(), type).ifPresent(versionColumn -> {
				if (count == 0) {
					throw new OptimisticLockException(context);
				} else {
					var columnMap = MappingUtils.getMappingColumnMap(metadata.getSchema(), type, SqlKind.NONE);
					var keys = metadata.getColumns().stream()
							.filter(TableMetadata.Column::isKey)
							.sorted(Comparator.comparingInt(TableMetadata.Column::getKeySeq))
							.map(c -> columnMap.get(c.getCamelColumnName()).getValue(entity))
							.toArray();

					find(type, keys).ifPresent(e -> versionColumn.setValue(entity, versionColumn.getValue(e)));
				}
			});
			return count;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_UPDATE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#updateAndReturn(java.lang.Object)
	 */
	@Override
	public <E> E updateAndReturn(final E entity) {
		update(entity);
		return entity;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#delete(java.lang.Class)
	 */
	@Override
	public <E> SqlEntityUpdate<E> update(final Class<? extends E> entityType) {
		var handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);

			var context = handler.createUpdateContext(this, metadata, entityType, false);
			context.setSqlKind(SqlKind.ENTITY_DELETE);

			return new SqlEntityUpdateImpl<>(this, handler, metadata, context);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_DELETE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#merge(java.lang.Object)
	 */
	@Override
	public <E> int merge(final E entity) {
		mergeAndReturn(entity);
		return 1;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#mergeAndReturn(java.lang.Object)
	 */
	@Override
	public <E> E mergeAndReturn(final E entity) {
		return doMergeAndReturn(entity, false);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#mergeWithLocking(java.lang.Object)
	 */
	@Override
	public <E> int mergeWithLocking(final E entity) {
		mergeWithLockingAndReturn(entity);
		return 1;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#mergeWithLockingAndReturn(java.lang.Object)
	 */
	@Override
	public <E> E mergeWithLockingAndReturn(final E entity) {
		return doMergeAndReturn(entity, true);
	}

	/**
	 * エンティティのMERGE（INSERTまたはUPDATE）を実行し、MERGEしたエンティティを返却する.<br>
	 * locking引数が<code>true</code>の場合、マージの最初に発行するEntityの検索で悲観ロック（forUpdateNoWait）を行う
	 *
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @param locking エンティティの悲観ロックを行うかどうか
	 * @return MERGEしたエンティティ
	 *
	 */
	@SuppressWarnings("unchecked")
	private <E> E doMergeAndReturn(final E entity, final boolean locking) {
		if (entity instanceof Stream) {
			throw new IllegalArgumentException("Stream type not supported.");
		}

		var handler = this.getEntityHandler();
		if (!handler.getEntityType().isInstance(entity)) {
			throw new IllegalArgumentException("Entity type not supported.");
		}

		var type = entity.getClass();
		try {
			var metadata = handler.getMetadata(this.transactionManager, type);
			var keyColumns = metadata.getKeyColumns();

			if (keyColumns.isEmpty()) {
				throw new IllegalArgumentException("Entity has no keys.");
			}

			var mappingColumns = MappingUtils.getMappingColumnMap(metadata.getSchema(), type, SqlKind.UPDATE);
			var query = (SqlEntityQuery<E>) query(type);
			for (var column : keyColumns) {
				var camelName = column.getCamelColumnName();
				query.equal(camelName, mappingColumns.get(camelName).getValue(entity));
			}
			if (locking) {
				query.forUpdateNoWait();
			}
			return query.first()
					.map(findEntity -> {
						for (var mappingColumn : mappingColumns.values()) {
							if (!mappingColumn.isId()) {
								var value = mappingColumn.getValue(entity);
								if (value != null) {
									mappingColumn.setValue(findEntity, value);
								}
							}
						}
						return updateAndReturn(findEntity);
					}).orElseGet(() -> insertAndReturn(entity));
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.MERGE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#delete(java.lang.Object)
	 */
	@Override
	public <E> int delete(final E entity) {
		if (entity instanceof Stream) {
			throw new IllegalArgumentException("Stream type not supported.");
		}

		var handler = this.getEntityHandler();
		if (!handler.getEntityType().isInstance(entity)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var type = entity.getClass();
			var metadata = handler.getMetadata(this.transactionManager, type);
			var context = handler.createDeleteContext(this, metadata, type, true);
			context.setSqlKind(SqlKind.ENTITY_DELETE);
			handler.setDeleteParams(context, entity);
			return handler.doDelete(this, context, entity);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_DELETE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#deleteAndReturn(java.lang.Object)
	 */
	@Override
	public <E> E deleteAndReturn(final E entity) {
		delete(entity);
		return entity;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#delete(java.lang.Class, java.lang.Object[])
	 */
	@Override
	public <E> int delete(final Class<? extends E> entityType, final Object... keys) {
		var handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			TableMetadata.Column keyColumn = null;
			var keyColumns = metadata.getKeyColumns();
			if (keyColumns.size() == 1) {
				keyColumn = keyColumns.get(0);
			} else if (keyColumns.isEmpty()) {
				keyColumn = metadata.getColumns().get(0);
			} else {
				throw new IllegalArgumentException("Entity has multiple keys");
			}
			return delete(entityType).in(keyColumn.getCamelColumnName(), keys).count();
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_DELETE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#delete(java.lang.Class)
	 */
	@Override
	public <E> SqlEntityDelete<E> delete(final Class<? extends E> entityType) {
		var handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = handler.createDeleteContext(this, metadata, entityType, false);
			context.setSqlKind(SqlKind.ENTITY_DELETE);
			return new SqlEntityDeleteImpl<>(this, handler, metadata, context);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_DELETE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#truncate(java.lang.Class)
	 */
	@Override
	public <E> SqlAgent truncate(final Class<? extends E> entityType) {
		var handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}
		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = this.context().setSql("truncate table " + metadata.getTableIdentifier());
			context.setSqlKind(SqlKind.TRUNCATE);
			update(context);
			return this;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.TRUNCATE, e);
		}
	}

	/**
	 * 自動採番カラムを取得する. 合わせて、{@link ExecutionContext#setGeneratedKeyColumns(String[])} に自動採番カラムを設定する.
	 * <pre>
	 * 自動採番カラムの条件
	 * 1. {@link MappingColumn#isId()} == true 、もしくは{@link TableMetadata.Column#isAutoincrement()} == true であること
	 * 2. 1.に加え、エンティティの対象フィールド型がprimitive型、もしくはフィールドの値が<code>null</code>であること
	 * </pre>
	 *
	 * @param <E> entity
	 * @param context ExecutionContext
	 * @param mappingColumns EntityのMappingColumn配列
	 * @param metadata TableMetadata
	 * @param entity Entity
	 * @return 自動採番カラム配列
	 */
	private <E> List<MappingColumn> getAutoGeneratedColumns(final ExecutionContext context,
			final MappingColumn[] mappingColumns,
			final TableMetadata metadata,
			final E entity) {
		var autoGeneratedColumns = new ArrayList<MappingColumn>();
		var autoGeneratedColumnNames = new ArrayList<String>();

		if (mappingColumns != null && mappingColumns.length > 0) {
			for (var column : metadata.getColumns()) {
				if (column.isAutoincrement()) {
					for (var mappingColumn : mappingColumns) {
						if (mappingColumn.getCamelName().equals(column.getCamelColumnName())) {
							// primitive型か、エンティティの対象フィールドがnullとなっている場合自動生成カラムとする
							if (mappingColumn.getJavaType().getRawType().isPrimitive()
									|| mappingColumn.getValue(entity) == null) {
								autoGeneratedColumns.add(mappingColumn);
								autoGeneratedColumnNames.add(column.getColumnName());
							}
							break;
						}
					}
				}
			}
			for (var mappingColumn : mappingColumns) {
				if (!mappingColumn.isId() || autoGeneratedColumns.contains(mappingColumn)) {
					continue;
				}
				for (var column : metadata.getColumns()) {
					if (mappingColumn.getCamelName().equals(column.getCamelColumnName())) {
						// primitive型か、エンティティの対象フィールドがnullとなっている場合自動生成カラムとする
						if (mappingColumn.getJavaType().getRawType().isPrimitive()
								|| mappingColumn.getValue(entity) == null) {
							autoGeneratedColumns.add(mappingColumn);
							autoGeneratedColumnNames.add(column.getColumnName());
						}
						break;
					}
				}
			}
		}

		if (!autoGeneratedColumnNames.isEmpty()) {
			context.setGeneratedKeyColumns(
					autoGeneratedColumnNames.toArray(new String[autoGeneratedColumnNames.size()]));
		}
		return autoGeneratedColumns;
	}

	/**
	 * entityにID値を設定する
	 * @param entity Entity
	 * @param id ID値
	 * @param column 設定する対象のカラム
	 */
	private void setEntityIdValue(final Object entity, final Object id, final MappingColumn column) {
		var rawType = column.getJavaType().getRawType();
		try {
			if (int.class.equals(rawType) || Integer.class.equals(rawType)) {
				if (id instanceof Number) {
					column.setValue(entity, ((Number) id).intValue());
				} else {
					throw new IllegalArgumentException();
				}
			} else if (long.class.equals(rawType) || Long.class.equals(rawType)) {
				if (id instanceof Number) {
					column.setValue(entity, ((Number) id).longValue());
				} else {
					throw new IllegalArgumentException();
				}
			} else if (BigInteger.class.equals(rawType)) {
				if (id instanceof BigInteger) {
					column.setValue(entity, id);
				} else if (id instanceof BigDecimal) {
					column.setValue(entity, ((BigDecimal) id).toBigInteger());
				} else if (id instanceof Number) {
					column.setValue(entity, new BigInteger(((Number) id).toString()));
				} else {
					throw new IllegalArgumentException();
				}
			} else if (BigDecimal.class.equals(rawType)) {
				if (id instanceof BigDecimal) {
					column.setValue(entity, id);
				} else if (id instanceof BigInteger) {
					column.setValue(entity, new BigDecimal((BigInteger) id));
				} else if (id instanceof Number) {
					column.setValue(entity, new BigDecimal(((Number) id).toString()));
				} else {
					throw new IllegalArgumentException();
				}
			} else if (String.class.equals(rawType)) {
				if (id instanceof String) {
					column.setValue(entity, id);
				} else if (id instanceof BigDecimal) {
					column.setValue(entity, ((BigDecimal) id).toPlainString());
				} else if (id instanceof Number) {
					column.setValue(entity, ((Number) id).toString());
				} else {
					column.setValue(entity, id.toString());
				}
			} else {
				try {
					column.setValue(entity, id);
				} catch (UroborosqlRuntimeException ex) {
					throw new UroborosqlRuntimeException(
							"Column is not correct as ID type. column=" + column.getCamelName() + ", type=" + rawType);
				}
			}
		} catch (IllegalArgumentException ex) {
			throw new UroborosqlRuntimeException(
					"Column type and ID type do not match. column=" + column.getCamelName() + ", type="
							+ rawType
							+ ", id=" + id + ", type=" + id.getClass().getSimpleName());
		}
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#getSqlConfig()
	 */
	@Override
	public SqlConfig getSqlConfig() {
		return this.sqlConfig;
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#context()
	 */
	@Override
	public ExecutionContext context() {
		return sqlConfig.context();
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
	public SqlAgent setFetchSize(final int fetchSize) {
		this.fetchSize = fetchSize;
		return this;
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
	public SqlAgent setQueryTimeout(final int queryTimeout) {
		this.queryTimeout = queryTimeout;
		return this;
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
	 * @return SqlAgent
	 */
	public SqlAgent setSqlRetryCodes(final List<String> sqlRetryCodes) {
		this.sqlRetryCodes = sqlRetryCodes;
		return this;
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
	 * @return SqlAgent
	 */
	public SqlAgent setMaxRetryCount(final int maxRetryCount) {
		this.maxRetryCount = maxRetryCount;
		return this;
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
	 * @return SqlAgent
	 */
	public SqlAgent setRetryWaitTime(final int retryWaitTime) {
		this.retryWaitTime = retryWaitTime;
		return this;
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#getMapKeyCaseFormat()
	 */
	@Override
	public CaseFormat getMapKeyCaseFormat() {
		return mapKeyCaseFormat;
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#setMapKeyCaseFormat(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public SqlAgent setMapKeyCaseFormat(final CaseFormat mapKeyCaseFormat) {
		this.mapKeyCaseFormat = mapKeyCaseFormat;
		return this;
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#getInsertsType()
	 */
	@Override
	public InsertsType getInsertsType() {
		return this.insertsType;
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#setInsertsType(jp.co.future.uroborosql.enums.InsertsType)
	 */
	@Override
	public SqlAgent setInsertsType(final InsertsType defaultInsertsType) {
		this.insertsType = defaultInsertsType;
		return this;
	}

	/**
	 * SQLリソース管理クラスを取得します。
	 *
	 * @return SQLリソース管理クラス
	 */
	private SqlResourceManager getSqlResourceManager() {
		return sqlConfig.getSqlResourceManager();
	}

	/**
	 * ORM処理クラス を取得します。
	 *
	 * @return ORM処理クラス
	 */
	@SuppressWarnings("unchecked")
	private <E> EntityHandler<E> getEntityHandler() {
		return (EntityHandler<E>) sqlConfig.getEntityHandler();
	}

	/**
	 * Dialect を取得します。
	 *
	 * @return Dialect
	 */
	private Dialect getDialect() {
		return sqlConfig.getDialect();
	}

	/**
	 * ステートメント初期化。
	 *
	 * @param executionContext ExecutionContext
	 * @return PreparedStatement
	 * @throws SQLException SQL例外
	 */
	private PreparedStatement getPreparedStatement(final ExecutionContext executionContext) throws SQLException {
		var stmt = ((LocalTransactionManager) transactionManager).getPreparedStatement(executionContext);
		// プロパティ設定
		applyProperties(stmt);
		return stmt;
	}

	/**
	 * Callableステートメント初期化
	 *
	 * @param executionContext ExecutionContext
	 * @return CallableStatement
	 * @throws SQLException SQL例外
	 */
	private CallableStatement getCallableStatement(final ExecutionContext executionContext) throws SQLException {
		var stmt = ((LocalTransactionManager) transactionManager).getCallableStatement(executionContext);
		// プロパティ設定
		applyProperties(stmt);
		return stmt;
	}

	/**
	 * フェッチサイズとクエリタイムアウトをPreparedStatementに設定する
	 *
	 * @param preparedStatement PreparedStatement
	 * @throws SQLException SQL例外
	 */
	private void applyProperties(final PreparedStatement preparedStatement) throws SQLException {
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
	 * 経過時間を計算し、HH:mm:ss.SSSSSSにフォーマットする.
	 *
	 * @param start 開始時間
	 * @param end 終了時間
	 * @return フォーマットした経過時間
	 */
	private static String formatElapsedTime(final Instant start, final Instant end) {
		return ELAPSED_TIME_FORMAT.format(LocalTime.MIDNIGHT.plus(Duration.between(start, end)));
	}

	/**
	 * ResultSetをStreamで扱うためのSpliterator
	 *
	 * @author H.Sugimoto
	 *
	 * @param <T> ResultSetの1行を変換した型
	 */
	private static final class ResultSetSpliterator<T> extends Spliterators.AbstractSpliterator<T> {
		private final ResultSetConverter<T> converter;
		private final ResultSet rs;
		private boolean finished = false;

		private ResultSetSpliterator(final ResultSet rs, final ResultSetConverter<T> converter) {
			super(Long.MAX_VALUE, Spliterator.ORDERED);
			this.rs = rs;
			this.converter = converter;
		}

		@Override
		public boolean tryAdvance(final Consumer<? super T> action) {
			try {
				if (finished || !rs.next()) {
					if (!rs.isClosed()) {
						rs.close();
					}
					finished = true;
					return false;
				}
				action.accept(converter.createRecord(rs));
				return true;
			} catch (RuntimeException | Error ex) {
				try {
					if (rs != null && !rs.isClosed()) {
						rs.close();
					}
				} catch (SQLException e) {
					e.printStackTrace();
				}
				throw ex;
			} catch (SQLException ex) {
				try {
					if (rs != null && !rs.isClosed()) {
						rs.close();
					}
				} catch (SQLException e) {
					e.printStackTrace();
				}
				throw new UroborosqlSQLException(ex);
			}
		}
	}

	/**
	 * ResultSetのラッパークラス。ResultSetのクローズに合わせてStatementもクローズする。
	 *
	 * @author H.Sugimoto
	 * @version 0.5.0
	 */
	private static class InnerResultSet extends AbstractResultSetWrapper {
		/** 同期してクローズするStatement */
		private final Statement stmt;

		/**
		 * コンストラクタ
		 *
		 * @param wrapped 元となるResultSet
		 * @param stmt Statement
		 */
		InnerResultSet(final ResultSet wrapped, final Statement stmt) {
			super(wrapped);
			this.stmt = stmt;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.AbstractResultSetWrapper#close()
		 */
		@Override
		public void close() throws SQLException {
			try {
				super.close();
			} finally {
				try {
					if (stmt != null && !stmt.isClosed()) {
						stmt.close();
					}
				} catch (SQLException e) {
					// do nothing
				}
			}
		}
	}
}
