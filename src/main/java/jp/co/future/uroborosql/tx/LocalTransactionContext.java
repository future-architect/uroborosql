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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.event.AfterCommitEvent;
import jp.co.future.uroborosql.event.AfterCreateCallableStatementEvent;
import jp.co.future.uroborosql.event.AfterCreatePreparedStatementEvent;
import jp.co.future.uroborosql.event.AfterRollbackEvent;
import jp.co.future.uroborosql.event.BeforeCommitEvent;
import jp.co.future.uroborosql.event.BeforeRollbackEvent;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.exception.UroborosqlTransactionException;

/**
 * ローカルトランザクションContext
 *
 * @author ota
 */
class LocalTransactionContext implements TransactionContext {
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
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#getSqlConfig()
	 */
	@Override
	public SqlConfig getSqlConfig() {
		return this.sqlConfig;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#getConnection()
	 */
	@Override
	public Connection getConnection() throws SQLException {
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
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#getConnectionContext()
	 */
	@Override
	public ConnectionContext getConnectionContext() {
		return this.connectionContext;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#getPreparedStatement(jp.co.future.uroborosql.context.ExecutionContext)
	 */
	@Override
	public PreparedStatement getPreparedStatement(final ExecutionContext executionContext) throws SQLException {
		var conn = getConnection();

		PreparedStatement stmt = null;
		switch (executionContext.getSqlKind()) {
		case INSERT:
		case ENTITY_INSERT:
		case BULK_INSERT:
		case BATCH_INSERT:
		case ENTITY_BULK_INSERT:
		case ENTITY_BATCH_INSERT:
			if (updatable) {
				// バッチ処理の場合、Dialectがバッチでの自動生成キー取得をサポートしているか確認
				var isBatchOperation = (executionContext.getSqlKind() == SqlKind.BATCH_INSERT
						|| executionContext.getSqlKind() == SqlKind.ENTITY_BATCH_INSERT);
				var supportsBatchKeys = sqlConfig.getDialect().supportsBatchGeneratedKeys();

				if ((supportsBatchKeys || !isBatchOperation) && executionContext.hasGeneratedKeyColumns()) {
					stmt = conn.prepareStatement(executionContext.getExecutableSql(),
							executionContext.getGeneratedKeyColumns());
				} else {
					stmt = conn.prepareStatement(executionContext.getExecutableSql());
				}
			} else {
				throw new UroborosqlTransactionException("Transaction not started.");
			}
			break;
		case SELECT:
		case ENTITY_SELECT:
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

		// PreparedStatement作成後イベント発行
		if (this.sqlConfig.getEventListenerHolder().hasAfterCreatePreparedStatementListener()) {
			var eventObj = new AfterCreatePreparedStatementEvent(executionContext, conn, stmt);
			for (var listener : this.sqlConfig.getEventListenerHolder().getAfterCreatePreparedStatementListeners()) {
				listener.accept(eventObj);
			}
			stmt = eventObj.getPreparedStatement();
		}
		return stmt;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#getCallableStatement(jp.co.future.uroborosql.context.ExecutionContext)
	 */
	@Override
	public CallableStatement getCallableStatement(final ExecutionContext executionContext) throws SQLException {
		var conn = getConnection();

		if (this.updatable) {
			var cstmt = conn.prepareCall(executionContext.getExecutableSql(),
					executionContext.getResultSetType(),
					executionContext.getResultSetConcurrency());

			// CallableStatement作成後イベント発行
			if (this.sqlConfig.getEventListenerHolder().hasAfterCreateCallableStatementListener()) {
				var eventObj = new AfterCreateCallableStatementEvent(executionContext, conn, cstmt);
				for (var listener : this.sqlConfig.getEventListenerHolder()
						.getAfterCreateCallableStatementListeners()) {
					listener.accept(eventObj);
				}
				cstmt = eventObj.getCallableStatement();

			}
			return cstmt;
		} else {
			throw new UroborosqlTransactionException("Transaction not started.");
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#commit()
	 */
	@Override
	public void commit() {
		try {
			if (connection != null && !connection.isClosed() && !connection.getAutoCommit()) {
				// コミット前イベント発行
				if (this.sqlConfig.getEventListenerHolder().hasBeforeCommitListener()) {
					var beforeEventObj = new BeforeCommitEvent(this, connection);
					this.sqlConfig.getEventListenerHolder().getBeforeCommitListeners()
							.forEach(listener -> listener.accept(beforeEventObj));
				}
				connection.commit();
				// コミット後イベント発行
				if (this.sqlConfig.getEventListenerHolder().hasAfterCommitListener()) {
					var afterEventObj = new AfterCommitEvent(this, connection);
					this.sqlConfig.getEventListenerHolder().getAfterCommitListeners()
							.forEach(listener -> listener.accept(afterEventObj));
				}
			}
		} catch (SQLException ex) {
			throw new UroborosqlSQLException(ex);
		}
		clearState();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#rollback()
	 */
	@Override
	public void rollback() {
		try {
			if (connection != null && !connection.isClosed() && !connection.getAutoCommit()) {
				// ロールバック前イベント発行
				if (this.sqlConfig.getEventListenerHolder().hasBeforeRollbackListener()) {
					var beforeEventObj = new BeforeRollbackEvent(this, connection);
					this.sqlConfig.getEventListenerHolder().getBeforeRollbackListeners()
							.forEach(listener -> listener.accept(beforeEventObj));
				}
				connection.rollback();
				// ロールバック後イベント発行
				if (this.sqlConfig.getEventListenerHolder().hasAfterRollbackListener()) {
					var afterEventObj = new AfterRollbackEvent(this, connection);
					this.sqlConfig.getEventListenerHolder().getAfterRollbackListeners()
							.forEach(listener -> listener.accept(afterEventObj));
				}
			}
		} catch (SQLException ex) {
			throw new UroborosqlSQLException(ex);
		}
		clearState();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#isRollbackOnly()
	 */
	@Override
	public boolean isRollbackOnly() {
		return rollbackOnly;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#setRollbackOnly()
	 */
	@Override
	public void setRollbackOnly() {
		this.rollbackOnly = true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#rollback(java.lang.String)
	 */
	@Override
	public void rollback(final String savepointName) {
		if (connection != null) {
			try {
				connection.rollback(savepointMap.get(savepointName));
			} catch (SQLException ex) {
				throw new UroborosqlSQLException(ex);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#setSavepoint(java.lang.String)
	 */
	@Override
	public void setSavepoint(final String savepointName) {
		if (savepointNames.contains(savepointName)) {
			throw new IllegalStateException();
		}
		savepointNames.add(savepointName);

		if (connection != null) {
			try {
				savepointMap.put(savepointName, connection.setSavepoint(savepointName));
			} catch (SQLException ex) {
				throw new UroborosqlSQLException(ex);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#releaseSavepoint(java.lang.String)
	 */
	@Override
	public void releaseSavepoint(final String savepointName) {
		var savepoint = savepointMap.get(savepointName);

		var pos = savepointNames.lastIndexOf(savepointName);
		if (pos > -1) {
			var subList = savepointNames.subList(pos, savepointNames.size());
			for (var name : subList) {
				savepointMap.remove(name);
			}
			subList.clear();
		}

		if (savepoint != null && connection != null) {
			try {
				connection.releaseSavepoint(savepoint);
			} catch (SQLException ex) {
				throw new UroborosqlSQLException(ex);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#getSavepointNames()
	 */
	@Override
	public List<String> getSavepointNames() {
		return Collections.unmodifiableList(this.savepointNames);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionContext#close()
	 */
	@Override
	public void close() {
		try {
			if (!isRollbackOnly()) {
				commit();
			} else {
				rollback();
			}
			if (connection != null && !connection.isClosed()) {
				connection.close();
			} else {
				warnWith(LOG)
						.log("Connection close was skipped because the connection was already closed.");
			}
		} catch (SQLException ex) {
			throw new UroborosqlSQLException(ex);
		}
		connection = null;
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
		for (var savepointName : savepointNames) {
			try {
				savepointMap.put(savepointName, connection.setSavepoint(savepointName));
			} catch (SQLException ex) {
				throw new UroborosqlSQLException(ex);
			}
		}
	}
}
