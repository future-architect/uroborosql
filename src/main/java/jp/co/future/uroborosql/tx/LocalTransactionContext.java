/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.tx;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Savepoint;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.exception.UroborosqlTransactionException;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.tx.cache.QueryCache;
import jp.co.future.uroborosql.tx.cache.StandardQueryCache;

/**
 * ローカルトランザクションContext
 *
 * @author ota
 */
class LocalTransactionContext implements AutoCloseable {

	/** セーブポイント名リスト */
	private final List<String> savepointNames = new ArrayList<>();

	/** セーブポイントキャッシュ */
	private final ConcurrentMap<String, Savepoint> savepointMap = new ConcurrentHashMap<>();

	/** SQL設定クラス */
	private final SqlConfig sqlConfig;

	/** コネクション */
	private Connection connection;

	/** ロールバックフラグ */
	private boolean rollbackOnly = false;

	/** 更新（INSERT/UPDATE/DELETE）SQL発行可能かどうか */
	private final boolean updatable;

	/** エンティティ型毎の検索結果キャッシュ */
	private Map<Class<?>, QueryCache<?>> queryCacheMap = null;

	/**
	 * コンストラクタ
	 *
	 * @param sqlConfig SQL設定クラス
	 * @param updatable 更新（INSERT/UPDATE/DELETE）SQL発行可能かどうか
	 */
	LocalTransactionContext(final SqlConfig sqlConfig, final boolean updatable) {
		this.sqlConfig = sqlConfig;
		this.updatable = updatable;
	}

	/**
	 * コネクションの取得
	 *
	 * @return コネクション
	 * @throws SQLException SQL例外
	 */
	Connection getConnection() throws SQLException {
		if (connection == null) {
			connection = this.sqlConfig.getConnectionSupplier().getConnection();
			initSavepoints(connection);
		}
		return connection;
	}

	/**
	 * コネクションの取得
	 *
	 * @param alias エイリアス名
	 * @return コネクション
	 * @throws SQLException SQL例外
	 */
	Connection getConnection(final String alias) throws SQLException {
		if (connection == null) {
			connection = this.sqlConfig.getConnectionSupplier().getConnection(alias);
			initSavepoints(connection);
		}
		return connection;
	}

	/**
	 * ステートメント取得
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return PreparedStatement
	 * @throws SQLException SQL例外
	 */
	PreparedStatement getPreparedStatement(final SqlContext sqlContext) throws SQLException {
		Connection conn = null;
		if (sqlContext.getDbAlias() != null) {
			conn = getConnection(sqlContext.getDbAlias());
		} else {
			conn = getConnection();
		}

		if (conn == null) {
			throw new IllegalArgumentException(sqlContext.getDbAlias());
		}

		PreparedStatement stmt = null;
		switch (sqlContext.getSqlKind()) {
		case INSERT:
		case ENTITY_INSERT:
		case BULK_INSERT:
		case BATCH_INSERT:
			if (updatable) {
				if (sqlContext.hasGeneratedKeyColumns()) {
					stmt = conn.prepareStatement(sqlContext.getExecutableSql(), sqlContext.getGeneratedKeyColumns());
				} else {
					stmt = conn.prepareStatement(sqlContext.getExecutableSql(), Statement.RETURN_GENERATED_KEYS);
				}
			} else {
				throw new UroborosqlTransactionException("Transaction not started.");
			}
			break;
		case SELECT:
		case ENTITY_SELECT:
			stmt = conn.prepareStatement(sqlContext.getExecutableSql(),
					sqlContext.getResultSetType(),
					sqlContext.getResultSetConcurrency());
			break;
		default:
			if (updatable) {
				stmt = conn.prepareStatement(sqlContext.getExecutableSql());
			} else {
				throw new UroborosqlTransactionException("Transaction not started.");
			}
			break;
		}
		return this.sqlConfig.getSqlFilterManager().doPreparedStatement(sqlContext, stmt);
	}

	/**
	 * Callableステートメント初期化
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return CallableStatement
	 * @throws SQLException SQL例外
	 */
	CallableStatement getCallableStatement(final SqlContext sqlContext) throws SQLException {
		Connection conn = null;
		if (sqlContext.getDbAlias() != null) {
			conn = getConnection(sqlContext.getDbAlias());
		} else {
			conn = getConnection();
		}
		if (conn == null) {
			throw new IllegalArgumentException(sqlContext.getDbAlias());
		}

		if (this.updatable) {
			return this.sqlConfig.getSqlFilterManager().doCallableStatement(sqlContext,
					conn.prepareCall(sqlContext.getExecutableSql(), sqlContext.getResultSetType(),
							sqlContext.getResultSetConcurrency()));
		} else {
			throw new UroborosqlTransactionException("Transaction not started.");
		}
	}

	/**
	 * トランザクションのセーブポイントを作成します。
	 *
	 * <pre>
	 *  この処理は{@link Connection#setSavepoint(String)}の処理に依存します。
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 * @throws SQLException SQL例外
	 */
	void setSavepoint(final String savepointName) {
		if (savepointNames.contains(savepointName)) {
			throw new IllegalStateException();
		}
		savepointNames.add(savepointName);

		if (connection != null) {
			try {
				savepointMap.put(savepointName, connection.setSavepoint(savepointName));
			} catch (SQLException e) {
				throw new UroborosqlSQLException(e);
			}
		}
	}

	/**
	 * トランザクションから指定されたセーブポイントと以降のセーブポイントを削除します。
	 *
	 * <pre>
	 *  この処理は{@link Connection#releaseSavepoint(java.sql.Savepoint)}の処理に依存します。
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 * @throws SQLException SQL例外
	 */
	void releaseSavepoint(final String savepointName) {
		Savepoint savepoint = savepointMap.get(savepointName);

		int pos = savepointNames.lastIndexOf(savepointName);
		if (pos > -1) {
			List<String> subList = savepointNames.subList(pos, savepointNames.size());
			for (String name : subList) {
				savepointMap.remove(name);
			}
			subList.clear();
		}

		if (savepoint != null && connection != null) {
			try {
				connection.releaseSavepoint(savepoint);
			} catch (SQLException e) {
				throw new UroborosqlSQLException(e);
			}
		}
	}

	/**
	 * 指定されたセーブポイントが設定されたあとに行われたすべての変更をロールバックします。
	 *
	 * <pre>
	 *  この処理は{@link Connection#rollback(java.sql.Savepoint)}の処理に依存します。
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 * @throws SQLException SQL例外
	 */
	void rollback(final String savepointName) {
		if (connection != null) {
			try {
				connection.rollback(savepointMap.get(savepointName));
			} catch (SQLException e) {
				throw new UroborosqlSQLException(e);
			}
		}
	}

	/**
	 * 現在のトランザクションをロールバックすることを予約します。
	 */
	void setRollbackOnly() {
		this.rollbackOnly = true;
	}

	/**
	 * 現在のトランザクションがロールバック指定になっているかを取得します。
	 */
	boolean isRollbackOnly() {
		return rollbackOnly;
	}

	/**
	 * トランザクションのコミット
	 *
	 * @throws SQLException SQL例外. トランザクションのコミットに失敗した場合
	 */
	void commit() {
		if (connection != null) {
			try {
				connection.commit();
			} catch (SQLException e) {
				throw new UroborosqlSQLException(e);
			}
		}
		clearState();
	}

	/**
	 * トランザクションのロールバック
	 *
	 * @throws SQLException SQL例外. トランザクションのロールバックに失敗した場合
	 */
	void rollback() {
		if (connection != null) {
			try {
				connection.rollback();
			} catch (SQLException e) {
				throw new UroborosqlSQLException(e);
			}
		}
		clearState();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.lang.AutoCloseable#close()
	 */
	@Override
	public void close() {
		if (queryCacheMap != null && !queryCacheMap.isEmpty()) {
			queryCacheMap.values().forEach(QueryCache::close);
			queryCacheMap.clear();
			queryCacheMap = null;
		}

		if (connection != null) {
			try {
				if (!isRollbackOnly()) {
					commit();
				} else {
					rollback();
				}
				connection.close();
			} catch (SQLException e) {
				throw new UroborosqlSQLException(e);
			}
			connection = null;
		}
	}

	/**
	 * 検索結果キャッシュの取得
	 *
	 * @param E エンティティ
	 * @param entityType エンティティ型
	 * @return 検索結果キャッシュ
	 */
	@SuppressWarnings("unchecked")
	public <E> QueryCache<E> getQueryCache(final Class<E> entityType, final TableMetadata metadata) {
		if (queryCacheMap == null) {
			queryCacheMap = new ConcurrentHashMap<>();
		}
		return (QueryCache<E>) queryCacheMap.computeIfAbsent(entityType, k -> {
			return new StandardQueryCache<>(entityType, metadata);
		});
	}

	@SuppressWarnings("unchecked")
	public <E> QueryCache<E> findQueryCache(final Class<E> entityType) {
		return (QueryCache<E>) queryCacheMap.get(entityType);
	}

	/**
	 * 検索結果としてキャッシュされているエンティティの型のSetを取得する.
	 *
	 * @return 検索結果としてキャッシュされているエンティティの型のSet
	 */
	public Set<Class<?>> getCacheEntityTypes() {
		if (queryCacheMap == null) {
			return Collections.emptySet();
		}
		return queryCacheMap.keySet();
	}

	/**
	 * 検索結果キャッシュの内容をコピーする
	 *
	 * @param original コピー元の検索結果キャッシュ
	 * @return コピーした検索結果キャッシュ
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public <E> void copyQueryCache(final QueryCache<E> original) {
		if (queryCacheMap == null) {
			queryCacheMap = new ConcurrentHashMap<>();
		}
		queryCacheMap.put(original.getEntityType(), new StandardQueryCache(original.getEntityType(), original));
	}

	/**
	 * ステータスクリア
	 */
	private void clearState() {
		savepointNames.clear();
		savepointMap.clear();
		rollbackOnly = false;
	}

	/**
	 * Savepointの設定を遅延して行う
	 *
	 * @param connection コネクション
	 * @throws SQLException SQL例外
	 */
	private void initSavepoints(final Connection connection) {
		for (String savepointName : savepointNames) {
			try {
				savepointMap.put(savepointName, connection.setSavepoint(savepointName));
			} catch (SQLException e) {
				throw new UroborosqlSQLException(e);
			}
		}
	}

}
