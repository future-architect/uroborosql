/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Clock;
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
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.context.ExecutionContextImpl;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.exception.OptimisticLockException;
import jp.co.future.uroborosql.exception.PessimisticLockException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.SqlEntityDelete;
import jp.co.future.uroborosql.fluent.SqlEntityUpdate;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.MappingColumn;
import jp.co.future.uroborosql.mapping.MappingUtils;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SQL実行用クラス。
 *
 * @author H.Sugimoto
 */
public class SqlAgentImpl extends AbstractAgent {
	/** ロガー */
	protected static final Logger LOG = LoggerFactory.getLogger(SqlAgentImpl.class);

	/** 経過時間のフォーマッタ */
	private static final DateTimeFormatter ELAPSED_TIME_FORMAT = DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS");

	/** 例外発生時のログ出力を行うかどうか デフォルトは<code>true</code> */
	protected boolean outputExceptionLog = true;

	/** IN句に渡すパラメータのMAXサイズ */
	protected static final int IN_CLAUSE_MAX_PARAM_SIZE = 1000;

	/**
	 * コンストラクタ。
	 *
	 * @param sqlConfig SQL設定管理クラス
	 * @param settings 設定情報
	 * @param connectionContext DB接続情報
	 */
	protected SqlAgentImpl(final SqlConfig sqlConfig, final Map<String, String> settings,
			final ConnectionContext connectionContext) {
		super(sqlConfig, settings, connectionContext);
		if (settings.containsKey(SqlAgentProviderImpl.PROPS_KEY_OUTPUT_EXCEPTION_LOG)) {
			outputExceptionLog = Boolean
					.parseBoolean(settings.get(SqlAgentProviderImpl.PROPS_KEY_OUTPUT_EXCEPTION_LOG));
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
		transformContext(executionContext, true);

		var stmt = getPreparedStatement(executionContext);

		// INパラメータ設定
		executionContext.bindParams(stmt);

		Instant startTime = null;
		if (LOG.isDebugEnabled()) {
			LOG.debug("Execute search SQL.");
			startTime = Instant.now(Clock.systemDefaultZone());
		}

		// 前処理
		beforeQuery(executionContext);

		try {
			// デフォルト最大リトライ回数を取得し、個別指定（ExecutionContextの値）があれば上書き
			var maxRetryCount = getMaxRetryCount();
			if (executionContext.getMaxRetryCount() > 0) {
				maxRetryCount = executionContext.getMaxRetryCount();
			}

			// デフォルトリトライ待機時間を取得し、個別指定（ExecutionContextの値）があれば上書き
			var retryWaitTime = getRetryWaitTime();
			if (executionContext.getRetryWaitTime() > 0) {
				retryWaitTime = executionContext.getRetryWaitTime();
			}
			var loopCount = 0;
			var dialect = getSqlConfig().getDialect();
			ResultSet rs = null;
			try {
				do {
					try {
						if (maxRetryCount > 0 && dialect.isRollbackToSavepointBeforeRetry()) {
							setSavepoint(RETRY_SAVEPOINT_NAME);
						}
						rs = new InnerResultSet(
								getSqlConfig().getSubscribers().queryResult(executionContext, stmt,
										stmt.executeQuery()),
								stmt);
						stmt.closeOnCompletion();
						return rs;
					} catch (SQLException ex) {
						if (maxRetryCount > 0 && dialect.isRollbackToSavepointBeforeRetry()) {
							rollback(RETRY_SAVEPOINT_NAME);
						}
						var errorCode = String.valueOf(ex.getErrorCode());
						var sqlState = ex.getSQLState();
						var pessimisticLockingErrorCodes = dialect.getPessimisticLockingErrorCodes();
						if (maxRetryCount > loopCount) {
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
							} else if (pessimisticLockingErrorCodes.contains(errorCode)
									|| pessimisticLockingErrorCodes.contains(sqlState)) {
								throw new PessimisticLockException(executionContext, ex);
							} else {
								throw ex;
							}
						} else if (pessimisticLockingErrorCodes.contains(errorCode)
								|| pessimisticLockingErrorCodes.contains(sqlState)) {
							throw new PessimisticLockException(executionContext, ex);
						} else {
							throw ex;
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
			afterQuery(executionContext);
			if (LOG.isDebugEnabled() && startTime != null) {
				LOG.debug("SQL execution time [{}({})] : [{}]", generateSqlName(executionContext),
						executionContext.getSqlKind(),
						formatElapsedTime(startTime, Instant.now(Clock.systemDefaultZone())));
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
	}

	/**
	 * SQL検索前処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param executionContext ExecutionContext
	 */
	protected void beforeQuery(final ExecutionContext executionContext) {
	}

	/**
	 * SQL検索後処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param executionContext ExecutionContext
	 */
	protected void afterQuery(final ExecutionContext executionContext) {
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
		final var rs = query(executionContext);
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
		try (Stream<Map<String, Object>> stream = query(executionContext,
				new MapResultSetConverter(getSqlConfig(), caseFormat))) {
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
		transformContext(executionContext, false);

		Instant startTime = null;

		try (var stmt = getPreparedStatement(executionContext)) {

			// INパラメータ設定
			executionContext.bindParams(stmt);

			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute update SQL.");
				startTime = Instant.now(Clock.systemDefaultZone());
			}

			// 前処理
			beforeUpdate(executionContext);

			// デフォルト最大リトライ回数を取得し、個別指定（ExecutionContextの値）があれば上書き
			var maxRetryCount = getMaxRetryCount();
			if (executionContext.getMaxRetryCount() > 0) {
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						setSavepoint(RETRY_SAVEPOINT_NAME);
					}
					var count = getSqlConfig().getSubscribers().updateResult(executionContext, stmt,
							stmt.executeUpdate());
					if ((SqlKind.INSERT.equals(executionContext.getSqlKind()) ||
							SqlKind.BULK_INSERT.equals(executionContext.getSqlKind()))
							&& executionContext.hasGeneratedKeyColumns()) {
						try (var rs = stmt.getGeneratedKeys()) {
							List<Object> generatedKeyValues = new ArrayList<>();
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
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
			afterUpdate(executionContext);
			if (LOG.isDebugEnabled() && startTime != null) {
				LOG.debug("SQL execution time [{}({})] : [{}]", generateSqlName(executionContext),
						executionContext.getSqlKind(),
						formatElapsedTime(startTime, Instant.now(Clock.systemDefaultZone())));
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
	}

	/**
	 * SQL更新前処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param executionContext ExecutionContext
	 */
	protected void beforeUpdate(final ExecutionContext executionContext) {
	}

	/**
	 * SQL更新後処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param executionContext ExecutionContext
	 */
	protected void afterUpdate(final ExecutionContext executionContext) {
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
		transformContext(executionContext, false);

		Instant startTime = null;

		try (var stmt = getPreparedStatement(executionContext)) {

			// INパラメータ設定
			executionContext.bindBatchParams(stmt);

			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute batch process.");
				startTime = Instant.now(Clock.systemDefaultZone());
			}

			// 前処理
			beforeBatch(executionContext);

			// デフォルト最大リトライ回数を取得し、個別指定（ExecutionContextの値）があれば上書き
			var maxRetryCount = getMaxRetryCount();
			if (executionContext.getMaxRetryCount() > 0) {
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						setSavepoint(RETRY_SAVEPOINT_NAME);
					}
					var counts = getSqlConfig().getSubscribers().batchResult(executionContext, stmt,
							stmt.executeBatch());
					if (SqlKind.BATCH_INSERT.equals(executionContext.getSqlKind())
							&& executionContext.hasGeneratedKeyColumns()) {
						try (var rs = stmt.getGeneratedKeys()) {
							List<Object> generatedKeyValues = new ArrayList<>();
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
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
			afterBatch(executionContext);
			if (LOG.isDebugEnabled() && startTime != null) {
				LOG.debug("SQL execution time [{}({})] : [{}]", generateSqlName(executionContext),
						executionContext.getSqlKind(),
						formatElapsedTime(startTime, Instant.now(Clock.systemDefaultZone())));
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
	}

	/**
	 * SQLバッチ前処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param executionContext ExecutionContext
	 */
	protected void beforeBatch(final ExecutionContext executionContext) {
	}

	/**
	 * SQLバッチ処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param executionContext ExecutionContext
	 */
	protected void afterBatch(final ExecutionContext executionContext) {
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
		transformContext(executionContext, false);

		Instant startTime = null;

		try (var callableStatement = getCallableStatement(executionContext)) {

			// パラメータ設定
			executionContext.bindParams(callableStatement);

			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute stored procedure.");
				startTime = Instant.now(Clock.systemDefaultZone());
			}

			beforeProcedure(executionContext);

			// デフォルト最大リトライ回数を取得し、個別指定（ExecutionContextの値）があれば上書き
			var maxRetryCount = getMaxRetryCount();
			if (executionContext.getMaxRetryCount() > 0) {
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						setSavepoint(RETRY_SAVEPOINT_NAME);
					}
					getSqlConfig().getSubscribers().procedureResult(executionContext, callableStatement,
							callableStatement.execute());
					break;
				} catch (SQLException ex) {
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
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
			afterProcedure(executionContext);
			if (LOG.isDebugEnabled() && startTime != null) {
				LOG.debug("Stored procedure execution time [{}({})] : [{}]", generateSqlName(executionContext),
						executionContext.getSqlKind(),
						formatElapsedTime(startTime, Instant.now(Clock.systemDefaultZone())));
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
		return null;
	}

	/**
	 * ストプロ実行前処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param executionContext ExecutionContext
	 */
	protected void beforeProcedure(final ExecutionContext executionContext) {

	}

	/**
	 * ストプロ実行後処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param executionContext ExecutionContext
	 */
	protected void afterProcedure(final ExecutionContext executionContext) {
	}

	/** 時間計測用のログに出力するSQL名を生成する.
	 *
	 * @param executionContext ExecutionContext
	 * @return SQL名. SQL名が取得できない場合はSQL_ID、または空文字を返却する
	 */
	protected String generateSqlName(final ExecutionContext executionContext) {
		if (executionContext.getSqlName() != null) {
			return executionContext.getSqlName();
		} else {
			return Objects.toString(executionContext.getSqlId(), "");
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.AbstractAgent#handleException(jp.co.future.uroborosql.context.ExecutionContext,
	 *      java.sql.SQLException)
	 */
	@Override
	protected void handleException(final ExecutionContext executionContext, final SQLException ex) throws SQLException {
		var cause = ex;

		while (cause.getNextException() != null) {
			cause = cause.getNextException();
		}

		if (LOG.isErrorEnabled() && isOutputExceptionLog()) {
			var builder = new StringBuilder();
			builder.append(System.lineSeparator()).append("Exception occurred in SQL execution.")
					.append(System.lineSeparator());
			builder.append("Executed SQL[").append(executionContext.getExecutableSql()).append("]")
					.append(System.lineSeparator());
			if (executionContext instanceof ExecutionContextImpl) {
				var bindParameters = ((ExecutionContextImpl) executionContext).getBindParameters();
				for (var i = 0; i < bindParameters.length; i++) {
					var parameter = getSqlConfig().getSubscribers().parameter(executionContext,
							bindParameters[i]);
					builder.append("Bind Parameter.[INDEX[").append(i + 1).append("], ").append(parameter.toString())
							.append("]").append(System.lineSeparator());
				}
			}
			LOG.error(builder.toString(), cause);
		}

		throw cause;
	}

	/**
	 * 例外発生時のログ出力を行うかどうかを取得します。
	 *
	 * @return 例外発生時のログ出力を行うかどうか
	 */
	protected boolean isOutputExceptionLog() {
		return outputExceptionLog;
	}

	/**
	 * 例外発生時のログ出力を行うかどうかを設定します。
	 *
	 * @param outputExceptionLog 例外発生時のログ出力を行うかどうか。ログ出力する場合は<code>true</code>
	 */
	protected void setOutputExceptionLog(final boolean outputExceptionLog) {
		this.outputExceptionLog = outputExceptionLog;
	}

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

			var keyNames = metadata.getColumns().stream().filter(TableMetadata.Column::isKey)
					.sorted(Comparator.comparingInt(TableMetadata.Column::getKeySeq))
					.map(TableMetadata.Column::getColumnName).map(CaseFormat.CAMEL_CASE::convert)
					.toArray(String[]::new);

			if (keyNames.length != keys.length) {
				throw new IllegalArgumentException("Number of keys does not match");
			}
			Map<String, Object> params = new HashMap<>();
			for (var i = 0; i < keys.length; i++) {
				params.put(keyNames[i], keys[i]);
			}

			var context = handler.createSelectContext(this, metadata, entityType, true);
			context.paramMap(params);

			try (Stream<E> stream = handler.doSelect(this, context, entityType)) {
				return stream.findFirst();
			}
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.SELECT, e);
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
			Class<?> entityType = entity.getClass();
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = handler.createInsertContext(this, metadata, entityType);
			context.setSqlKind(SqlKind.INSERT);

			// 自動採番カラムの取得とcontextへの設定を行う
			var mappingColumns = MappingUtils.getMappingColumns(entityType);
			List<MappingColumn> autoGeneratedColumns = getAutoGeneratedColumns(context, mappingColumns, metadata,
					entity);

			getSqlConfig().getSubscribers().insertParams(context, entity);
			handler.setInsertParams(context, entity);
			var count = handler.doInsert(this, context, entity);

			if (!autoGeneratedColumns.isEmpty()) {
				var ids = context.getGeneratedKeyValues();
				var idx = 0;
				for (MappingColumn col : autoGeneratedColumns) {
					var id = ids[idx++];
					setEntityIdValue(entity, id, col);
				}
			}

			return count;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.INSERT, e);
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
	protected <E> List<MappingColumn> getAutoGeneratedColumns(final ExecutionContext context,
			final MappingColumn[] mappingColumns,
			final TableMetadata metadata, final E entity) {
		List<MappingColumn> autoGeneratedColumns = new ArrayList<>();
		List<String> autoGeneratedColumnNames = new ArrayList<>();

		if (mappingColumns != null && mappingColumns.length > 0) {
			for (TableMetadata.Column column : metadata.getColumns()) {
				if (column.isAutoincrement()) {
					for (MappingColumn mappingColumn : mappingColumns) {
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
			for (MappingColumn mappingColumn : mappingColumns) {
				if (!mappingColumn.isId() || autoGeneratedColumns.contains(mappingColumn)) {
					continue;
				}
				for (TableMetadata.Column column : metadata.getColumns()) {
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
	protected void setEntityIdValue(final Object entity, final Object id, final MappingColumn column) {
		Class<?> rawType = column.getJavaType().getRawType();
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
			Class<?> type = entity.getClass();
			var metadata = handler.getMetadata(this.transactionManager, type);
			var context = handler.createUpdateContext(this, metadata, type, true);
			context.setSqlKind(SqlKind.UPDATE);
			getSqlConfig().getSubscribers().updateParams(context, entity);
			handler.setUpdateParams(context, entity);
			var count = handler.doUpdate(this, context, entity);

			MappingUtils.getVersionMappingColumn(type).ifPresent(versionColumn -> {
				if (count == 0) {
					throw new OptimisticLockException(context);
				} else {
					var columnMap = MappingUtils.getMappingColumnMap(type, SqlKind.NONE);
					var keys = metadata.getColumns().stream().filter(TableMetadata.Column::isKey)
							.sorted(Comparator.comparingInt(TableMetadata.Column::getKeySeq))
							.map(c -> {
								var col = columnMap.get(c.getCamelColumnName());
								return col.getValue(entity);
							}).toArray();

					find(type, keys).ifPresent(e -> {
						versionColumn.setValue(entity, versionColumn.getValue(e));
					});
				}
			});
			return count;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.UPDATE, e);
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
	@SuppressWarnings("unchecked")
	@Override
	public <E> SqlEntityUpdate<E> update(final Class<? extends E> entityType) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);

			var context = handler.createUpdateContext(this, metadata, entityType, false);
			context.setSqlKind(SqlKind.UPDATE);

			return new SqlEntityUpdateImpl<>(this, handler, metadata, context);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.DELETE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#delete(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <E> int delete(final E entity) {
		if (entity instanceof Stream) {
			throw new IllegalArgumentException("Stream type not supported.");
		}

		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isInstance(entity)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			Class<?> type = entity.getClass();
			var metadata = handler.getMetadata(this.transactionManager, type);
			var context = handler.createDeleteContext(this, metadata, type, true);
			context.setSqlKind(SqlKind.DELETE);
			getSqlConfig().getSubscribers().deleteParams(context, entity);
			handler.setDeleteParams(context, entity);
			return handler.doDelete(this, context, entity);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.DELETE, e);
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
	@SuppressWarnings("unchecked")
	@Override
	public <E> int delete(final Class<? extends E> entityType, final Object... keys) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			TableMetadata.Column keyColumn = null;
			List<? extends TableMetadata.Column> keyColumns = metadata.getKeyColumns();
			if (keyColumns.size() == 1) {
				keyColumn = keyColumns.get(0);
			} else if (keyColumns.isEmpty()) {
				keyColumn = metadata.getColumns().get(0);
			} else {
				throw new IllegalArgumentException("Entity has multiple keys");
			}
			return delete(entityType).in(keyColumn.getCamelColumnName(), keys).count();
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.DELETE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#delete(java.lang.Class)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <E> SqlEntityDelete<E> delete(final Class<? extends E> entityType) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);

			var context = handler.createDeleteContext(this, metadata, entityType, false);
			context.setSqlKind(SqlKind.DELETE);

			return new SqlEntityDeleteImpl<>(this, handler, metadata, context);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.DELETE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#truncate(java.lang.Class)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <E> SqlAgent truncate(final Class<? extends E> entityType) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}
		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = this.contextWith("truncate table " + metadata.getTableIdentifier());
			context.setSqlKind(SqlKind.TRUNCATE);
			update(context);
			return this;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.TRUNCATE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.AbstractAgent#batchInsert(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition, java.util.List)
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected <E> int batchInsert(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition, final List<E> insertedEntities) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = handler.createBatchInsertContext(this, metadata, entityType);
			context.setSqlKind(SqlKind.BATCH_INSERT);

			var count = 0;
			List<E> entityList = new ArrayList<>();
			var isFirst = true;
			List<MappingColumn> autoGeneratedColumns = Collections.emptyList();
			Map<String, Object> nonNullObjectIdFlags = null;
			for (var iterator = entities.iterator(); iterator.hasNext();) {
				var entity = iterator.next();

				if (!entityType.isInstance(entity)) {
					throw new IllegalArgumentException("Entity types do not match");
				}

				if (isFirst) {
					isFirst = false;
					var mappingColumns = MappingUtils.getMappingColumns(entityType);
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

				getSqlConfig().getSubscribers().insertParams(context, entity);
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
			throw new EntitySqlRuntimeException(SqlKind.BATCH_INSERT, e);
		}
	}

	protected <E> int[] doBatchInsert(final ExecutionContext context, final EntityHandler<E> handler,
			final List<E> entityList, final List<MappingColumn> autoGeneratedColumns) throws SQLException {
		var counts = handler.doBatchInsert(this, context);
		if (!autoGeneratedColumns.isEmpty()) {
			var ids = context.getGeneratedKeyValues();
			var idx = 0;
			for (E ent : entityList) {
				for (MappingColumn col : autoGeneratedColumns) {
					setEntityIdValue(ent, ids[idx++], col);
				}
			}
		}

		return counts;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.AbstractAgent#bulkInsert(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition, java.util.List)
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected <E> int bulkInsert(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition, final List<E> insertedEntities) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = handler.createBulkInsertContext(this, metadata, entityType);
			context.setSqlKind(SqlKind.BULK_INSERT);

			var frameCount = 0;
			var count = 0;
			var isFirst = true;
			List<MappingColumn> autoGeneratedColumns = Collections.emptyList();
			Map<String, Object> nonNullObjectIdFlags = null;
			List<E> entityList = new ArrayList<>();
			for (var iterator = entities.iterator(); iterator.hasNext();) {
				var entity = iterator.next();

				if (!entityType.isInstance(entity)) {
					throw new IllegalArgumentException("Entity types do not match");
				}

				if (isFirst) {
					isFirst = false;
					var mappingColumns = MappingUtils.getMappingColumns(entityType);
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

				getSqlConfig().getSubscribers().bulkInsertParams(context, entity, frameCount);
				handler.setBulkInsertParams(context, entity, frameCount);
				frameCount++;

				if (condition.test(context, frameCount, entity)) {
					count += doBulkInsert(context, entityType, handler, metadata, autoGeneratedColumns, entityList);
					frameCount = 0;
					entityList.clear();

					// 新しいExecutionContextを作成する前にgeneratedKeyColumnsを退避しておく
					var generatedKeyColumns = context.getGeneratedKeyColumns();
					context = handler.createBulkInsertContext(this, metadata, entityType);
					context.setSqlKind(SqlKind.BULK_INSERT);
					// 実行結果から生成されたIDを取得できるようにPreparedStatementにIDカラムを渡す
					context.setGeneratedKeyColumns(generatedKeyColumns);
				}
			}
			return count + (frameCount > 0
					? doBulkInsert(context, entityType, handler, metadata, autoGeneratedColumns, entityList)
					: 0);

		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.BULK_INSERT, e);
		}
	}

	protected <E> int doBulkInsert(final ExecutionContext context, final Class<E> entityType,
			final EntityHandler<E> handler,
			final TableMetadata metadata, final List<MappingColumn> autoGeneratedColumns, final List<E> entityList)
			throws SQLException {
		var count = handler.doBulkInsert(this,
				handler.setupSqlBulkInsertContext(this, context, metadata, entityType, entityList.size()));

		if (!autoGeneratedColumns.isEmpty()) {
			var ids = context.getGeneratedKeyValues();
			var idx = 0;
			for (E ent : entityList) {
				for (MappingColumn col : autoGeneratedColumns) {
					setEntityIdValue(ent, ids[idx++], col);
				}
			}
		}

		return count;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.AbstractAgent#batchUpdate(java.lang.Class, java.util.stream.Stream, jp.co.future.uroborosql.SqlAgent.UpdatesCondition, java.util.List)
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected <E> int batchUpdate(final Class<E> entityType, final Stream<E> entities,
			final UpdatesCondition<? super E> condition, final List<E> updatedEntities) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			var metadata = handler.getMetadata(this.transactionManager, entityType);
			var context = handler.createBatchUpdateContext(this, metadata, entityType);
			context.setSqlKind(SqlKind.BATCH_UPDATE);

			var versionColumn = updatedEntities != null
					? MappingUtils.getVersionMappingColumn(entityType)
					: null;

			var count = 0;
			List<E> entityList = new ArrayList<>();
			for (var iterator = entities.iterator(); iterator.hasNext();) {
				var entity = iterator.next();

				if (!entityType.isInstance(entity)) {
					throw new IllegalArgumentException("Entity types do not match");
				}

				entityList.add(entity);
				if (updatedEntities != null) {
					updatedEntities.add(entity);
				}

				getSqlConfig().getSubscribers().updateParams(context, entity);
				handler.setUpdateParams(context, entity);
				context.addBatch();

				if (condition.test(context, context.batchCount(), entity)) {
					count += Arrays.stream(handler.doBatchUpdate(this, context)).sum();
					entityList.clear();
				}
			}
			count = count + (context.batchCount() != 0
					? Arrays.stream(handler.doBatchUpdate(this, context)).sum()
					: 0);

			if (updatedEntities != null && versionColumn.isPresent()) {
				var vColumn = versionColumn.get();
				List<MappingColumn> keyColumns = metadata.getColumns().stream()
						.filter(TableMetadata.Column::isKey)
						.sorted(Comparator.comparingInt(TableMetadata.Column::getKeySeq))
						.map(c -> MappingUtils.getMappingColumnMap(entityType, SqlKind.NONE)
								.get(c.getCamelColumnName()))
						.collect(Collectors.toList());

				if (keyColumns.size() == 1) {
					// 単一キーの場合はIN句で更新した行を一括取得し@Versionのついたフィールドを更新する
					var keyColumn = keyColumns.get(0);
					Map<Object, List<E>> updatedEntityMap = updatedEntities.stream()
							.collect(Collectors.groupingBy(e -> keyColumn.getValue(e)));

					// updatedEntitiesのサイズが大きいとin句の上限にあたるため、1000件ずつに分割して検索する
					List<Object> keyList = new ArrayList<>(updatedEntityMap.keySet());
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
			return count;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.BATCH_UPDATE, e);
		}
	}

	/**
	 * ResultSetをStreamで扱うためのSpliterator
	 *
	 * @author H.Sugimoto
	 *
	 * @param <T> ResultSetの1行を変換した型
	 */
	private final class ResultSetSpliterator<T> extends Spliterators.AbstractSpliterator<T> {
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
					rs.close();
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

}
