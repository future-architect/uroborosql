package jp.co.future.uroborosql;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextImpl;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.exception.OptimisticLockException;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.MappingUtils;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.store.SqlManager;
import jp.co.future.uroborosql.utils.CaseFormat;

import org.apache.commons.lang3.time.StopWatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

/**
 * SQL実行用クラス。
 *
 * @author H.Sugimoto
 */
public class SqlAgentImpl extends AbstractAgent {
	/** ロガー */
	protected static final Logger LOG = LoggerFactory.getLogger(SqlAgentImpl.class);

	/** 例外発生時のログ出力を行うかどうか デフォルトは<code>true</code> */
	private boolean outputExceptionLog = true;

	/**
	 * コンストラクタ。
	 *
	 * @param sqlConfig SQL設定管理クラス
	 * @param defaultProps 初期化用プロパティ
	 */
	protected SqlAgentImpl(final SqlConfig sqlConfig, final Map<String, String> defaultProps) {
		super(sqlConfig, defaultProps);
		if (defaultProps.containsKey(SqlAgentFactoryImpl.PROPS_KEY_OUTPUT_EXCEPTION_LOG)) {
			outputExceptionLog = Boolean.parseBoolean(defaultProps
					.get(SqlAgentFactoryImpl.PROPS_KEY_OUTPUT_EXCEPTION_LOG));
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(jp.co.future.uroborosql.context.SqlContext)
	 */
	@Override
	public ResultSet query(final SqlContext sqlContext) throws SQLException {
		// パラメータログを出力する
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, Boolean.FALSE.toString());

		// コンテキスト変換
		transformContext(sqlContext);

		PreparedStatement stmt = getPreparedStatement(sqlContext);
		stmt.closeOnCompletion();

		// INパラメータ設定
		sqlContext.bindParams(stmt);

		StopWatch watch = null;
		if (LOG.isDebugEnabled()) {
			LOG.debug("Execute search SQL.");
			watch = new StopWatch();
			watch.start();
		}

		// 前処理
		beforeQuery(sqlContext);

		try {
			// デフォルト最大リトライ回数を取得し、個別指定（SqlContextの値）があれば上書き
			int maxRetryCount = getMaxRetryCount();
			if (sqlContext.getMaxRetryCount() > 0) {
				maxRetryCount = sqlContext.getMaxRetryCount();
			}

			// デフォルトリトライ待機時間を取得し、個別指定（SqlContextの値）があれば上書き
			int retryWaitTime = getRetryWaitTime();
			if (sqlContext.getRetryWaitTime() > 0) {
				retryWaitTime = sqlContext.getRetryWaitTime();
			}
			int loopCount = 0;
			do {
				try {
					ResultSet rs = getSqlFilterManager().doQuery(sqlContext, stmt, stmt.executeQuery());
					return rs;
				} catch (SQLException ex) {
					if (maxRetryCount > loopCount) {
						String errorCode = Integer.toString(ex.getErrorCode());
						if (getSqlRetryCodes().contains(errorCode)) {
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
					sqlContext.contextAttrs().put("__retryCount", loopCount);
				}
			} while (maxRetryCount > loopCount++);
			return null;
		} catch (SQLException ex) {
			handleException(sqlContext, ex);
			return null;
		} finally {
			// 後処理
			afterQuery(sqlContext);
			if (LOG.isDebugEnabled() && watch != null) {
				watch.stop();
				LOG.debug("SQL execution time [{}] : [{}]", sqlContext.getSqlName(), watch.toString());
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
	}

	/**
	 * SQL検索前処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param sqlContext SQLコンテキスト
	 */
	protected void beforeQuery(final SqlContext sqlContext) {
	}

	/**
	 * SQL検索後処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param sqlContext SQLコンテキスト
	 */
	protected void afterQuery(final SqlContext sqlContext) {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(jp.co.future.uroborosql.context.SqlContext,
	 *      jp.co.future.uroborosql.converter.ResultSetConverter)
	 */
	@Override
	public <T> Stream<T> query(final SqlContext sqlContext, final ResultSetConverter<T> converter) throws SQLException {
		Stream<T> stream = StreamSupport.stream(new Spliterators.AbstractSpliterator<T>(Long.MAX_VALUE,
				Spliterator.ORDERED | Spliterator.CONCURRENT) {
			private ResultSet rs;

			@Override
			public boolean tryAdvance(final Consumer<? super T> action) {
				try {
					if (rs == null) {
						rs = query(sqlContext);
					}
					if (!rs.next()) {
						rs.close();
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
		}, false);

		return stream;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#query(jp.co.future.uroborosql.context.SqlContext,
	 *      jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public List<Map<String, Object>> query(final SqlContext sqlContext, final CaseFormat caseFormat)
			throws SQLException {
		return query(sqlContext, new MapResultSetConverter(caseFormat)).collect(Collectors.toList());
	}

	/**
	 * @see jp.co.future.uroborosql.SqlAgent#update(jp.co.future.uroborosql.context.SqlContext)
	 */
	@Override
	public int update(final SqlContext sqlContext) throws SQLException {
		// パラメータログを出力する
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, Boolean.FALSE.toString());

		// コンテキスト変換
		transformContext(sqlContext);

		StopWatch watch = null;

		try (PreparedStatement stmt = getPreparedStatement(sqlContext)) {

			// INパラメータ設定
			sqlContext.bindParams(stmt);

			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute update SQL.");
				watch = new StopWatch();
				watch.start();
			}

			// 前処理
			beforeUpdate(sqlContext);

			// デフォルト最大リトライ回数を取得し、個別指定（SqlContextの値）があれば上書き
			int maxRetryCount = getMaxRetryCount();
			if (sqlContext.getMaxRetryCount() > 0) {
				maxRetryCount = sqlContext.getMaxRetryCount();
			}

			// デフォルトリトライ待機時間を取得し、個別指定（SqlContextの値）があれば上書き
			int retryWaitTime = getRetryWaitTime();
			if (sqlContext.getRetryWaitTime() > 0) {
				retryWaitTime = sqlContext.getRetryWaitTime();
			}
			int loopCount = 0;
			do {
				try {
					int result = getSqlFilterManager().doUpdate(sqlContext, stmt, stmt.executeUpdate());
					return result;
				} catch (SQLException ex) {
					if (maxRetryCount > loopCount) {
						String errorCode = Integer.toString(ex.getErrorCode());
						if (getSqlRetryCodes().contains(errorCode)) {
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
					sqlContext.contextAttrs().put("__retryCount", loopCount);
				}
			} while (maxRetryCount > loopCount++);
			return 0;
		} catch (SQLException ex) {
			handleException(sqlContext, ex);
			return 0;
		} finally {
			afterUpdate(sqlContext);
			if (LOG.isDebugEnabled() && watch != null) {
				watch.stop();
				LOG.debug("SQL execution time [{}] : [{}]", sqlContext.getSqlName(), watch.toString());
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
	}

	/**
	 * SQL更新前処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param sqlContext SQLコンテキスト
	 */
	protected void beforeUpdate(final SqlContext sqlContext) {
	}

	/**
	 * SQL更新後処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param sqlContext SQLコンテキスト
	 */
	protected void afterUpdate(final SqlContext sqlContext) {
	}

	/**
	 * @see jp.co.future.uroborosql.SqlAgent#batch(jp.co.future.uroborosql.context.SqlContext)
	 */
	@Override
	public int[] batch(final SqlContext sqlContext) throws SQLException {
		// バッチ処理の場合大量のログが出力されるため、パラメータログの出力を抑止する
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, Boolean.TRUE.toString());

		// コンテキスト変換
		transformContext(sqlContext);

		StopWatch watch = null;

		try (PreparedStatement stmt = getPreparedStatement(sqlContext)) {

			// INパラメータ設定
			sqlContext.bindBatchParams(stmt);

			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute batch process.");
				watch = new StopWatch();
				watch.start();
			}

			// 前処理
			beforeBatch(sqlContext);

			// デフォルト最大リトライ回数を取得し、個別指定（SqlContextの値）があれば上書き
			int maxRetryCount = getMaxRetryCount();
			if (sqlContext.getMaxRetryCount() > 0) {
				maxRetryCount = sqlContext.getMaxRetryCount();
			}

			// デフォルトリトライ待機時間を取得し、個別指定（SqlContextの値）があれば上書き
			int retryWaitTime = getRetryWaitTime();
			if (sqlContext.getRetryWaitTime() > 0) {
				retryWaitTime = sqlContext.getRetryWaitTime();
			}
			int loopCount = 0;
			do {
				try {
					int[] result = getSqlFilterManager().doBatch(sqlContext, stmt, stmt.executeBatch());
					sqlContext.clearBatch();
					return result;
				} catch (SQLException ex) {
					if (maxRetryCount > loopCount) {
						String errorCode = Integer.toString(ex.getErrorCode());
						if (getSqlRetryCodes().contains(errorCode)) {
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
					sqlContext.contextAttrs().put("__retryCount", loopCount);
				}
			} while (maxRetryCount > loopCount++);
			return null;
		} catch (SQLException ex) {
			handleException(sqlContext, ex);
			return null;
		} finally {
			// 後処理
			afterBatch(sqlContext);
			if (LOG.isDebugEnabled() && watch != null) {
				watch.stop();
				LOG.debug("SQL execution time [{}] : [{}]", sqlContext.getSqlName(), watch.toString());
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
	}

	/**
	 * SQLバッチ前処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param sqlContext SQLコンテキスト
	 */
	protected void beforeBatch(final SqlContext sqlContext) {
	}

	/**
	 * SQLバッチ処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param sqlContext SQLコンテキスト
	 */
	protected void afterBatch(final SqlContext sqlContext) {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#procedure(jp.co.future.uroborosql.context.SqlContext)
	 */
	@Override
	public Map<String, Object> procedure(final SqlContext sqlContext) throws SQLException {
		// パラメータログを出力する
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, Boolean.FALSE.toString());

		// コンテキスト変換
		transformContext(sqlContext);

		StopWatch watch = null;

		try (CallableStatement callableStatement = getCallableStatement(sqlContext)) {

			// パラメータ設定
			sqlContext.bindParams(callableStatement);

			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute stored procedure.");
				watch = new StopWatch();
				watch.start();
			}

			beforeProcedure(sqlContext);

			// デフォルト最大リトライ回数を取得し、個別指定（SqlContextの値）があれば上書き
			int maxRetryCount = getMaxRetryCount();
			if (sqlContext.getMaxRetryCount() > 0) {
				maxRetryCount = sqlContext.getMaxRetryCount();
			}

			// デフォルトリトライ待機時間を取得し、個別指定（SqlContextの値）があれば上書き
			int retryWaitTime = getRetryWaitTime();
			if (sqlContext.getRetryWaitTime() > 0) {
				retryWaitTime = sqlContext.getRetryWaitTime();
			}
			int loopCount = 0;
			do {
				try {
					getSqlFilterManager().doProcedure(sqlContext, callableStatement, callableStatement.execute());
					sqlContext.contextAttrs().put("__retryCount", loopCount);
					break;
				} catch (SQLException ex) {
					if (maxRetryCount > loopCount) {
						String errorCode = Integer.toString(ex.getErrorCode());
						if (getSqlRetryCodes().contains(errorCode)) {
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
					sqlContext.contextAttrs().put("__retryCount", loopCount);
				}
			} while (maxRetryCount > loopCount++);
			// 結果取得
			return sqlContext.getOutParams(callableStatement);
		} catch (SQLException ex) {
			handleException(sqlContext, ex);
		} finally {
			afterProcedure(sqlContext);
			if (LOG.isDebugEnabled() && watch != null) {
				watch.stop();
				LOG.debug("Stored procedure execution time [{}] : [{}]", sqlContext.getSqlName(), watch.toString());
			}
			MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
		}
		return null;
	}

	/**
	 * ストプロ実行前処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param sqlContext SQLコンテキスト
	 */
	protected void beforeProcedure(final SqlContext sqlContext) {

	}

	/**
	 * ストプロ実行後処理 拡張ポイント。子クラスでオーバーライドする
	 *
	 * @param sqlContext SQLコンテキスト
	 */
	protected void afterProcedure(final SqlContext sqlContext) {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.AbstractAgent#handleException(jp.co.future.uroborosql.context.SqlContext,
	 *      java.sql.SQLException)
	 */
	@Override
	protected void handleException(final SqlContext sqlContext, final SQLException ex) throws SQLException {
		SQLException cause = ex;

		while (cause.getNextException() != null) {
			cause = cause.getNextException();
		}

		if (LOG.isErrorEnabled() && isOutputExceptionLog()) {
			StringBuilder builder = new StringBuilder();
			builder.append(System.lineSeparator()).append("Exception occurred in SQL execution.")
					.append(System.lineSeparator());
			builder.append("Executed SQL[").append(sqlContext.getExecutableSql()).append("]")
					.append(System.lineSeparator());
			if (sqlContext instanceof SqlContextImpl) {
				Parameter[] bindParameters = ((SqlContextImpl) sqlContext).getBindParameters();
				for (int i = 0; i < bindParameters.length; i++) {
					Parameter parameter = getSqlFilterManager().doParameter(bindParameters[i]);
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
			TableMetadata metadata = handler.getMetadata(this.transactionManager, entityType);

			String[] keyNames = metadata.getColumns().stream().filter(TableMetadata.Column::isKey)
					.sorted(Comparator.comparingInt(TableMetadata.Column::getKeySeq))
					.map(TableMetadata.Column::getColumnName).map(CaseFormat.CAMEL_CASE::convert)
					.toArray(String[]::new);

			if (keyNames.length != keys.length) {
				throw new IllegalArgumentException("Number of keys does not match");
			}
			Map<String, Object> params = new HashMap<>();
			for (int i = 0; i < keys.length; i++) {
				params.put(keyNames[i], keys[i]);
			}

			SqlContext context = handler.createSelectContext(this, metadata, entityType);
			context.paramMap(params);

			return handler.doSelect(this, context, entityType).findFirst();
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(EntitySqlRuntimeException.EntityProcKind.SELECT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#insert(Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public int insert(final Object entity) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isInstance(entity)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			Class<?> type = entity.getClass();
			TableMetadata metadata = handler.getMetadata(this.transactionManager, type);
			SqlContext context = handler.createInsertContext(this, metadata, type);
			handler.setInsertParams(context, entity);
			return handler.doInsert(this, context, entity);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(EntitySqlRuntimeException.EntityProcKind.INSERT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#update(Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public int update(final Object entity) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isInstance(entity)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			Class<?> type = entity.getClass();
			TableMetadata metadata = handler.getMetadata(this.transactionManager, type);
			SqlContext context = handler.createUpdateContext(this, metadata, type);
			handler.setUpdateParams(context, entity);
			int count = handler.doUpdate(this, context, entity);

			if (count == 0 && MappingUtils.getVersionMappingColumn(type).isPresent()) {
				throw new OptimisticLockException(context);
			}
			return count;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(EntitySqlRuntimeException.EntityProcKind.UPDATE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgent#delete(Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public int delete(final Object entity) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isInstance(entity)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			Class<?> type = entity.getClass();
			TableMetadata metadata = handler.getMetadata(this.transactionManager, type);
			SqlContext context = handler.createDeleteContext(this, metadata, type);
			handler.setDeleteParams(context, entity);
			return handler.doDelete(this, context, entity);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(EntitySqlRuntimeException.EntityProcKind.DELETE, e);
		}
	}

}
