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
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.filter.SqlFilterManager;

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

	/** コネクション提供クラス */
	private final ConnectionSupplier connectionSupplier;

	/** SQLフィルタ管理クラス */
	private final SqlFilterManager sqlFilterManager;

	/** コネクション */
	private Connection connection;

	/** ロールバックフラグ */
	private boolean rollbackOnly = false;

	/**
	 * コンストラクタ
	 *
	 * @param connectionSupplier コネクション提供クラス
	 */
	LocalTransactionContext(final ConnectionSupplier connectionSupplier, final SqlFilterManager sqlFilterManager) {
		this.connectionSupplier = connectionSupplier;
		this.sqlFilterManager = sqlFilterManager;
	}

	/**
	 * コネクションの取得
	 *
	 * @return コネクション
	 * @throws SQLException SQL例外
	 */
	Connection getConnection() throws SQLException {
		if (connection == null) {
			connection = connectionSupplier.getConnection();
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
			connection = connectionSupplier.getConnection(alias);
			initSavepoints(connection);
		}
		return connection;
	}

	/**
	 * ステートメント初期化。
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
		case BULK_INSERT:
		case BATCH_INSERT:
			if (sqlContext.hasGeneratedKeyColumns()) {
				stmt = conn.prepareStatement(sqlContext.getExecutableSql(), sqlContext.getGeneratedKeyColumns());
			} else {
				stmt = conn.prepareStatement(sqlContext.getExecutableSql(), Statement.RETURN_GENERATED_KEYS);
			}
			break;
		case SELECT:
			stmt = conn.prepareStatement(sqlContext.getExecutableSql(),
					sqlContext.getResultSetType(),
					sqlContext.getResultSetConcurrency());
			break;
		default:
			stmt = conn.prepareStatement(sqlContext.getExecutableSql());
			break;
		}
		return sqlFilterManager.doPreparedStatement(sqlContext, stmt);
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

		CallableStatement stmt = sqlFilterManager.doCallableStatement(
				sqlContext,
				conn.prepareCall(sqlContext.getExecutableSql(), sqlContext.getResultSetType(),
						sqlContext.getResultSetConcurrency()));
		return stmt;
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
	 * ステータスクリア
	 */
	void clearState() {
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
