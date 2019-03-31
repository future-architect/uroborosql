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
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.apache.commons.lang3.time.StopWatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextImpl;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.enums.GenerationType;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.exception.OptimisticLockException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.SqlEntityDelete;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.MappingColumn;
import jp.co.future.uroborosql.mapping.MappingUtils;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.mapping.annotations.SequenceGenerator;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.utils.CaseFormat;

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
		transformContext(sqlContext, true);

		PreparedStatement stmt = getPreparedStatement(sqlContext);

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
			ResultSet rs = null;
			try {
				do {
					try {
						if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
							setSavepoint(RETRY_SAVEPOINT_NAME);
						}
						rs = new InnerResultSet(getSqlFilterManager().doQuery(sqlContext, stmt, stmt.executeQuery()),
								stmt);
						stmt.closeOnCompletion();
						return rs;
					} catch (SQLException ex) {
						if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
							rollback(RETRY_SAVEPOINT_NAME);
						}
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
						if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
							releaseSavepoint(RETRY_SAVEPOINT_NAME);
						}
						sqlContext.contextAttrs().put(CTX_ATTR_KEY_RETRY_COUNT, loopCount);
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
		final ResultSet rs = query(sqlContext);
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
	 * @see jp.co.future.uroborosql.SqlAgent#query(jp.co.future.uroborosql.context.SqlContext,
	 *      jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public List<Map<String, Object>> query(final SqlContext sqlContext, final CaseFormat caseFormat)
			throws SQLException {
		try (Stream<Map<String, Object>> stream = query(sqlContext,
				new MapResultSetConverter(getSqlConfig().getDialect(), caseFormat))) {
			return stream.collect(Collectors.toList());
		}
	}

	/**
	 * @see jp.co.future.uroborosql.SqlAgent#update(jp.co.future.uroborosql.context.SqlContext)
	 */
	@Override
	public int update(final SqlContext sqlContext) throws SQLException {
		// パラメータログを出力する
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, Boolean.FALSE.toString());

		// コンテキスト変換
		transformContext(sqlContext, false);

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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						setSavepoint(RETRY_SAVEPOINT_NAME);
					}
					int count = getSqlFilterManager().doUpdate(sqlContext, stmt, stmt.executeUpdate());
					if (SqlKind.INSERT.equals(sqlContext.getSqlKind()) && sqlContext.getGeneratedKeyColumns() != null
							&& sqlContext.getGeneratedKeyColumns().length > 0) {
						try (ResultSet rs = stmt.getGeneratedKeys()) {
							if (rs.next()) {
								List<BigDecimal> generatedKeyValues = new ArrayList<>();
								for (int i = 0; i < sqlContext.getGeneratedKeyColumns().length; i++) {
									generatedKeyValues.add(rs.getBigDecimal(i + 1));
								}
								sqlContext.setGeneratedKeyValues(
										generatedKeyValues.toArray(new BigDecimal[generatedKeyValues.size()]));
							}
						}
					}
					return count;
				} catch (SQLException ex) {
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						rollback(RETRY_SAVEPOINT_NAME);
					}
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						releaseSavepoint(RETRY_SAVEPOINT_NAME);
					}
					sqlContext.contextAttrs().put(CTX_ATTR_KEY_RETRY_COUNT, loopCount);
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
		transformContext(sqlContext, false);

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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						setSavepoint(RETRY_SAVEPOINT_NAME);
					}
					return getSqlFilterManager().doBatch(sqlContext, stmt, stmt.executeBatch());
				} catch (SQLException ex) {
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						rollback(RETRY_SAVEPOINT_NAME);
					}
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						releaseSavepoint(RETRY_SAVEPOINT_NAME);
					}
					sqlContext.clearBatch();
					sqlContext.contextAttrs().put(CTX_ATTR_KEY_RETRY_COUNT, loopCount);
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

		// procedureやfunctionの場合、SQL文法エラーになるためバインドパラメータコメントを出力しない
		sqlContext.contextAttrs().put(CTX_ATTR_KEY_OUTPUT_BIND_COMMENT, false);

		// コンテキスト変換
		transformContext(sqlContext, false);

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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						setSavepoint(RETRY_SAVEPOINT_NAME);
					}
					getSqlFilterManager().doProcedure(sqlContext, callableStatement, callableStatement.execute());
					break;
				} catch (SQLException ex) {
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						rollback(RETRY_SAVEPOINT_NAME);
					}
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
					if (maxRetryCount > 0 && getSqlConfig().getDialect().isRollbackToSavepointBeforeRetry()) {
						releaseSavepoint(RETRY_SAVEPOINT_NAME);
					}
					sqlContext.contextAttrs().put(CTX_ATTR_KEY_RETRY_COUNT, loopCount);
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

			SqlContext context = handler.createSelectContext(this, metadata, entityType, true);
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
			context.setSqlKind(SqlKind.INSERT);

			Dialect dialect = getSqlConfig().getDialect();

			// IDアノテーションが付与されたカラム情報を取得する
			Set<MappingColumn> idColumns = Arrays.stream(MappingUtils.getMappingColumns(type))
					.filter(c -> c.isId()).collect(Collectors.toSet());

			// GenerationType.IDENTITYの場合は、実行結果から生成されたIDを取得できるようにPreparedStatementにIDカラムを渡す
			MappingColumn[] identityKeyColumns = idColumns.stream()
					.filter(c -> GenerationType.IDENTITY.equals(c.getGeneratedValue().strategy()))
					.toArray(MappingColumn[]::new);
			if (identityKeyColumns.length > 0) {
				if (!dialect.supportsIdentity()) {
					// identityによる自動採番をサポートしていない場合は例外をスローする
					throw new UroborosqlRuntimeException("Database does not support AUTO_INCREMENT by IDENTITY.");
				}
				context.setGeneratedKeyColumns(
						Arrays.stream(identityKeyColumns).map(MappingColumn::getName).toArray(String[]::new));
			}

			// GenerationType.SEQUENCEの場合は、シーケンスを取得してentityに設定
			MappingColumn[] sequenceKeyColumns = idColumns.stream()
					.filter(c -> GenerationType.SEQUENCE.equals(c.getGeneratedValue().strategy()))
					.toArray(MappingColumn[]::new);
			if (sequenceKeyColumns.length > 0) {
				if (!dialect.supportsSequence()) {
					// sequenceによる自動採番をサポートしていない場合は例外をスローする
					throw new UroborosqlRuntimeException("Database does not support AUTO_INCREMENT by SEQUENCE.");
				}
				for (MappingColumn sequenceColumn : sequenceKeyColumns) {
					SequenceGenerator generator = sequenceColumn.getSequenceGenerator();
					if (generator == null) {
						throw new UroborosqlRuntimeException("SequenceGenerator has not been specified.");
					}
					String sequenceName = getQualifiedSequenceName(generator);
					String seqSql = dialect.getSequenceNextValSql(sequenceName);
					try (ResultSet rs = queryWith(seqSql).resultSet()) {
						if (rs.next()) {
							BigDecimal id = rs.getBigDecimal(1);
							setEntityIdValue(entity, id, sequenceColumn);
						}
					}
				}
			}

			handler.setInsertParams(context, entity);
			int count = handler.doInsert(this, context, entity);

			if (identityKeyColumns.length > 0) {
				BigDecimal[] ids = context.getGeneratedKeyValues();
				if (ids.length == identityKeyColumns.length) {
					for (int i = 0; i < identityKeyColumns.length; i++) {
						BigDecimal id = ids[i];
						MappingColumn column = identityKeyColumns[i];
						setEntityIdValue(entity, id, column);
					}
				}
			}

			return count;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.INSERT, e);
		}
	}

	/**
	 * entityにID値を設定する
	 * @param entity Entity
	 * @param id ID値
	 * @param column 設定する対象のカラム
	 */
	private void setEntityIdValue(final Object entity, final BigDecimal id, final MappingColumn column) {
		Class<?> rawType = column.getJavaType().getRawType();
		if (int.class.equals(rawType) || Integer.class.equals(rawType)) {
			column.setValue(entity, id.intValue());
		} else if (long.class.equals(rawType) || Long.class.equals(rawType)) {
			column.setValue(entity, id.longValue());
		} else if (BigInteger.class.equals(rawType)) {
			column.setValue(entity, id.toBigInteger());
		} else if (BigDecimal.class.equals(rawType)) {
			column.setValue(entity, id);
		} else if (String.class.equals(rawType)) {
			column.setValue(entity, id.toPlainString());
		}
	}

	/**
	 * 修飾済みシーケンス名を取得する
	 * @param generator SequenceGenerator
	 * @return 修飾済みシーケンス名
	 */
	private String getQualifiedSequenceName(final SequenceGenerator generator) {
		StringBuilder builder = new StringBuilder();
		if (!generator.catalog().isEmpty()) {
			builder.append(generator.catalog()).append(".");
		}
		if (!generator.schema().isEmpty()) {
			builder.append(generator.schema()).append(".");
		}
		builder.append(generator.sequence());
		return builder.toString();
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
			SqlContext context = handler.createUpdateContext(this, metadata, type, true);
			context.setSqlKind(SqlKind.UPDATE);
			handler.setUpdateParams(context, entity);
			int count = handler.doUpdate(this, context, entity);

			if (count == 0 && MappingUtils.getVersionMappingColumn(type).isPresent()) {
				throw new OptimisticLockException(context);
			}
			return count;
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.UPDATE, e);
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
			SqlContext context = handler.createDeleteContext(this, metadata, type, true);
			context.setSqlKind(SqlKind.DELETE);
			handler.setDeleteParams(context, entity);
			return handler.doDelete(this, context, entity);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.DELETE, e);
		}
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
			TableMetadata metadata = handler.getMetadata(this.transactionManager, entityType);
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
			TableMetadata metadata = handler.getMetadata(this.transactionManager, entityType);

			SqlContext context = handler.createDeleteContext(this, metadata, entityType, false);
			context.setSqlKind(SqlKind.DELETE);

			return new SqlEntityDeleteImpl<>(this, handler, metadata, context);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.DELETE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.AbstractAgent#batchInsert(Class, Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <E> int batchInsert(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			TableMetadata metadata = handler.getMetadata(this.transactionManager, entityType);
			SqlContext context = handler.createBatchInsertContext(this, metadata, entityType);
			context.setSqlKind(SqlKind.BATCH_INSERT);

			int count = 0;
			for (Iterator<E> iterator = entities.iterator(); iterator.hasNext();) {
				E entity = iterator.next();

				if (!entityType.isInstance(entity)) {
					throw new IllegalArgumentException("Entity types do not match");
				}

				handler.setInsertParams(context, entity);
				context.addBatch();

				count += condition.test(context, context.batchCount(), entity)
						? Arrays.stream(handler.doBatchInsert(this, context)).sum()
						: 0;
			}
			return count + (context.batchCount() != 0
					? Arrays.stream(handler.doBatchInsert(this, context)).sum()
					: 0);

		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.BATCH_INSERT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.AbstractAgent#bulkInsert(Class, Stream, jp.co.future.uroborosql.SqlAgent.InsertsCondition)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <E> int bulkInsert(final Class<E> entityType, final Stream<E> entities,
			final InsertsCondition<? super E> condition) {
		@SuppressWarnings("rawtypes")
		EntityHandler handler = this.getEntityHandler();
		if (!handler.getEntityType().isAssignableFrom(entityType)) {
			throw new IllegalArgumentException("Entity type not supported");
		}

		try {
			TableMetadata metadata = handler.getMetadata(this.transactionManager, entityType);
			SqlContext context = handler.createBulkInsertContext(this, metadata, entityType);
			context.setSqlKind(SqlKind.BULK_INSERT);

			int frameCount = 0;
			int count = 0;
			for (Iterator<E> iterator = entities.iterator(); iterator.hasNext();) {
				E entity = iterator.next();

				if (!entityType.isInstance(entity)) {
					throw new IllegalArgumentException("Entity types do not match");
				}

				handler.setBulkInsertParams(context, entity, frameCount);
				frameCount++;

				if (condition.test(context, frameCount, entity)) {
					count += doBulkInsert(context, entityType, handler, metadata, frameCount);
					frameCount = 0;
					context = handler.createBulkInsertContext(this, metadata, entityType);
					context.setSqlKind(SqlKind.BULK_INSERT);
				}
			}
			return count + (frameCount > 0 ? doBulkInsert(context, entityType, handler, metadata, frameCount) : 0);

		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.BULK_INSERT, e);
		}
	}

	private <E> int doBulkInsert(final SqlContext context, final Class<E> entityType, final EntityHandler<E> handler,
			final TableMetadata metadata, final int frameCount) throws SQLException {
		return handler.doBulkInsert(this,
				handler.setupSqlBulkInsertContext(this, context, metadata, entityType, frameCount));
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

}
