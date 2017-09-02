package jp.co.future.uroborosql.tx;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Deque;
import java.util.concurrent.ConcurrentLinkedDeque;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.filter.SqlFilterManager;

/**
 * ローカルトランザクションマネージャ
 *
 * @author ota
 */
public class LocalTransactionManager implements TransactionManager {
	/** コネクション供給クラス */
	private final ConnectionSupplier connectionSupplier;

	/** SQLフィルタ管理クラス */
	private final SqlFilterManager sqlFilterManager;

	/** トランザクションコンテキストのスタック */
	private final Deque<LocalTransactionContext> txCtxStack = new ConcurrentLinkedDeque<LocalTransactionContext>();

	/** トランザクション管理外の接続に利用する便宜上のトランザクション */
	private LocalTransactionContext unmanagedTransaction = null;

	/**
	 * コンストラクタ
	 *
	 * @param connectionSupplier コネクション供給クラス
	 * @param sqlFilterManager SQLフィルタ管理クラス
	 */
	public LocalTransactionManager(final ConnectionSupplier connectionSupplier, final SqlFilterManager sqlFilterManager) {
		this.connectionSupplier = connectionSupplier;
		this.sqlFilterManager = sqlFilterManager;

	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void required(final SQLRunnable runnable) throws SQLException {
		requiredInternal(toSupplier(runnable));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R required(final SQLSupplier<R> supplier) throws SQLException {
		return requiredInternal(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void requiresNew(final SQLRunnable runnable) throws SQLException {
		requiresNewInternal(toSupplier(runnable));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R requiresNew(final SQLSupplier<R> supplier) throws SQLException {
		return requiresNewInternal(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void notSupported(final SQLRunnable runnable) throws SQLException {
		notSupportedInternal(toSupplier(runnable));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R notSupported(final SQLSupplier<R> supplier) throws SQLException {
		return notSupportedInternal(supplier);
	}

	/**
	 * 現在有効なTransactionContextを取得する
	 *
	 * @return 現在有効なTransactionContext
	 */
	private LocalTransactionContext currentTxContext() {
		return txCtxStack.peek();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#setRollbackOnly()
	 */
	@Override
	public void setRollbackOnly() {
		currentTxContext().setRollbackOnly();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#setSavepoint(java.lang.String)
	 */
	@Override
	public void setSavepoint(final String savepointName) throws SQLException {
		currentTxContext().setSavepoint(savepointName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#releaseSavepoint(java.lang.String)
	 */
	@Override
	public void releaseSavepoint(final String savepointName) throws SQLException {
		currentTxContext().releaseSavepoint(savepointName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#rollback(java.lang.String)
	 */
	@Override
	public void rollback(final String savepointName) throws SQLException {
		currentTxContext().rollback(savepointName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#getConnection()
	 */
	@Override
	public Connection getConnection() {
		LocalTransactionContext txContext = currentTxContext();
		try {
			if (txContext == null) {
				return unmanagedTransaction != null ? unmanagedTransaction.getConnection()
						: (unmanagedTransaction = new LocalTransactionContext(connectionSupplier, sqlFilterManager))
								.getConnection();
			} else {
				return txContext.getConnection();
			}
		} catch (SQLException ex) {
			ex.printStackTrace();
			return null;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#getConnection(java.lang.String)
	 */
	@Override
	public Connection getConnection(final String alias) {
		LocalTransactionContext txContext = currentTxContext();
		try {
			if (txContext == null) {
				return unmanagedTransaction != null ? unmanagedTransaction.getConnection(alias)
						: (unmanagedTransaction = new LocalTransactionContext(connectionSupplier, sqlFilterManager))
								.getConnection(alias);
			}
			return txContext.getConnection(alias);
		} catch (SQLException ex) {
			ex.printStackTrace();
			return null;
		}
	}

	/**
	 * ステートメント初期化。
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return PreparedStatement
	 * @throws SQLException SQL例外
	 */
	public PreparedStatement getPreparedStatement(final SqlContext sqlContext) throws SQLException {
		LocalTransactionContext txContext = currentTxContext();
		if (txContext == null) {
			return unmanagedTransaction != null ? unmanagedTransaction.getPreparedStatement(sqlContext)
					: (unmanagedTransaction = new LocalTransactionContext(connectionSupplier, sqlFilterManager))
							.getPreparedStatement(sqlContext);
		} else {
			return txContext.getPreparedStatement(sqlContext);
		}
	}

	/**
	 * Callableステートメント初期化
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return CallableStatement
	 * @throws SQLException SQL例外
	 */
	public CallableStatement getCallableStatement(final SqlContext sqlContext) throws SQLException {
		LocalTransactionContext txContext = currentTxContext();
		if (txContext == null) {
			return unmanagedTransaction != null ? unmanagedTransaction.getCallableStatement(sqlContext)
					: (unmanagedTransaction = new LocalTransactionContext(connectionSupplier, sqlFilterManager))
							.getCallableStatement(sqlContext);
		} else {
			return txContext.getCallableStatement(sqlContext);
		}
	}

	/**
	 * 処理ブロックの実行
	 *
	 * @param block 処理ブロック
	 * @return SQLサプライヤ
	 */
	private SQLSupplier<Void> toSupplier(final SQLRunnable block) {
		return () -> {
			block.run();
			return null;
		};
	}

	/**
	 * requiredメソッドの内部実装
	 * @param supplier SQLサプライヤ
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	private <R> R requiredInternal(final SQLSupplier<R> supplier) throws SQLException {
		if (currentTxContext() != null) {
			return supplier.get();
		} else {
			return runInNewTx(supplier);
		}
	}

	/**
	 * requiresNewメソッドの内部実装
	 * @param supplier SQLサプライヤ
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	private <R> R requiresNewInternal(final SQLSupplier<R> supplier) throws SQLException {
		return runInNewTx(supplier);
	}

	/**
	 * notSupportedメソッドの内部実装
	 * @param supplier SQLサプライヤ
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	private <R> R notSupportedInternal(final SQLSupplier<R> supplier) throws SQLException {
		LocalTransactionContext txContext = currentTxContext();
		if (txContext != null) {
			// トランザクションをサスペンド
			txCtxStack.pop();
			try {
				return supplier.get();
			} finally {
				//戻す
				txCtxStack.push(txContext);
			}
		} else {
			return supplier.get();
		}
	}

	/**
	 * 新しいトランザクションを開始して処理を実行
	 *
	 * @param supplier トランザクション内で実行する処理
	 * @param <R> 結果の型
	 * @return 処理の結果
	 * @throws SQLException
	 */
	private <R> R runInNewTx(final SQLSupplier<R> supplier) throws SQLException {
		try (LocalTransactionContext txContext = new LocalTransactionContext(connectionSupplier, sqlFilterManager)) {
			txCtxStack.push(txContext);
			try {
				R result = supplier.get();
				return result;
			} catch (SQLException ex) {
				txContext.setRollbackOnly();
				throw ex;
			} catch (Exception ex) {
				txContext.setRollbackOnly();
				throw ex;
			} finally {
				try {
					if (!txContext.isRollbackOnly()) {
						txContext.commit();
					} else {
						txContext.rollback();
					}
					txContext.close();
				} finally {
					txCtxStack.pop();
				}
			}
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#close()
	 */
	@Override
	public void close() {
		txCtxStack.forEach((elem) -> elem.close());
		txCtxStack.clear();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#commit()
	 */
	@Override
	public void commit() throws SQLException {
		LocalTransactionContext txContext = currentTxContext();
		if (txContext == null) {
			if (unmanagedTransaction != null) {
				unmanagedTransaction.commit();
			}
		} else {
			txContext.commit();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#rollback()
	 */
	@Override
	public void rollback() throws SQLException {
		LocalTransactionContext txContext = currentTxContext();
		if (txContext == null) {
			if (unmanagedTransaction != null) {
				unmanagedTransaction.rollback();
			}
		} else {
			txContext.rollback();
		}
	}

}