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
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.exception.UroborosqlTransactionException;

/**
 * ローカルトランザクションContext
 *
 * @author ota
 */
public class LocalTransactionContext implements AutoCloseable {

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

	/** トランザクション内での更新を強制するかどうか */
	private final boolean updatable;

	/** DB接続情報. ConnectionSupplierで指定したデフォルトの接続情報を使用する場合は<code>null</code>を指定する */
	private final ConnectionContext connectionContext;

	/**
	 * コンストラクタ
	 *
	 * @param sqlConfig SQL設定クラス
	 * @param updatable 更新（INSERT/UPDATE/DELETE）SQL発行可能かどうか
	 * @param connectionContext DB接続情報. ConnectionSupplierで指定したデフォルトの接続情報を使用する場合は<code>null</code>を指定する
	 */
	LocalTransactionContext(final SqlConfig sqlConfig, final boolean updatable,
			final ConnectionContext connectionContext) {
		this.sqlConfig = sqlConfig;
		this.updatable = updatable;
		this.connectionContext = connectionContext;
	}

	/**
	 * コネクションの取得
	 *
	 * @return コネクション
	 * @throws SQLException SQL例外
	 */
	Connection getConnection() throws SQLException {
		if (connection == null) {
			if (connectionContext == null) {
				connection = this.sqlConfig.getConnectionSupplier().getConnection();
			} else {
				connection = this.sqlConfig.getConnectionSupplier().getConnection(connectionContext);
			}
			initSavepoints(connection);
		}
		return connection;
	}

	/**
	 * ステートメント取得
	 *
	 * @param executionContext ExecutionContext
	 * @return PreparedStatement
	 * @throws SQLException SQL例外
	 */
	PreparedStatement getPreparedStatement(final ExecutionContext executionContext) throws SQLException {
		var conn = getConnection();

		PreparedStatement stmt = null;
		switch (executionContext.getSqlKind()) {
		case INSERT:
		case BULK_INSERT:
		case BATCH_INSERT:
			if (updatable) {
				if (executionContext.hasGeneratedKeyColumns()) {
					stmt = conn.prepareStatement(executionContext.getExecutableSql(), executionContext.getGeneratedKeyColumns());
				} else {
					stmt = conn.prepareStatement(executionContext.getExecutableSql(), Statement.RETURN_GENERATED_KEYS);
				}
			} else {
				throw new UroborosqlTransactionException("Transaction not started.");
			}
			break;
		case SELECT:
			stmt = conn.prepareStatement(executionContext.getExecutableSql(),
					executionContext.getResultSetType(),
					executionContext.getResultSetConcurrency());
			break;
		default:
			if (updatable) {
				stmt = conn.prepareStatement(executionContext.getExecutableSql());
			} else {
				throw new UroborosqlTransactionException("Transaction not started.");
			}
			break;
		}
		return this.sqlConfig.getSubscribers().preparedStatement(executionContext, stmt);
	}

	/**
	 * Callableステートメント初期化
	 *
	 * @param executionContext ExecutionContext
	 * @return CallableStatement
	 * @throws SQLException SQL例外
	 */
	CallableStatement getCallableStatement(final ExecutionContext executionContext) throws SQLException {
		var conn = getConnection();

		if (this.updatable) {
			return this.sqlConfig.getSubscribers().callableStatement(executionContext,
					conn.prepareCall(executionContext.getExecutableSql(), executionContext.getResultSetType(),
							executionContext.getResultSetConcurrency()));
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
		var savepoint = savepointMap.get(savepointName);

		var pos = savepointNames.lastIndexOf(savepointName);
		if (pos > -1) {
			var subList = savepointNames.subList(pos, savepointNames.size());
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
				sqlConfig.getSubscribers().beforeCommit(this, Optional.ofNullable(connectionContext));
				connection.commit();
				sqlConfig.getSubscribers().afterCommit(this, Optional.ofNullable(connectionContext));
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
				sqlConfig.getSubscribers().beforeRollback(this, Optional.ofNullable(connectionContext));
				connection.rollback();
				sqlConfig.getSubscribers().afterRollback(this, Optional.ofNullable(connectionContext));
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
