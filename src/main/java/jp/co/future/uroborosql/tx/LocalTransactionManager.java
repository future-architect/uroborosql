package jp.co.future.uroborosql.tx;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Deque;
import java.util.Optional;
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
	private final Deque<LocalTransactionContext> txCtxStack = new ConcurrentLinkedDeque<>();

	/** トランザクション管理外の接続に利用する便宜上のトランザクション */
	private Optional<LocalTransactionContext> unmanagedTransaction = Optional.empty();

	/**
	 * コンストラクタ
	 *
	 * @param connectionSupplier コネクション供給クラス
	 * @param sqlFilterManager SQLフィルタ管理クラス
	 */
	public LocalTransactionManager(final ConnectionSupplier connectionSupplier,
			final SqlFilterManager sqlFilterManager) {
		this.connectionSupplier = connectionSupplier;
		this.sqlFilterManager = sqlFilterManager;

	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void required(final SQLRunnable runnable) {
		requiredInternal(toSupplier(runnable));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R required(final SQLSupplier<R> supplier) {
		return requiredInternal(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void requiresNew(final SQLRunnable runnable) {
		requiresNewInternal(toSupplier(runnable));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R requiresNew(final SQLSupplier<R> supplier) {
		return requiresNewInternal(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void notSupported(final SQLRunnable runnable) {
		notSupportedInternal(toSupplier(runnable));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R notSupported(final SQLSupplier<R> supplier) {
		return notSupportedInternal(supplier);
	}

	/**
	 * 現在有効なTransactionContextを取得する
	 *
	 * @return 現在有効なTransactionContext
	 */
	private Optional<LocalTransactionContext> currentTxContext() {
		return Optional.ofNullable(this.txCtxStack.peek());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#setRollbackOnly()
	 */
	@Override
	public void setRollbackOnly() {
		Optional<LocalTransactionContext> txContext = currentTxContext();
		if (txContext.isPresent()) {
			txContext.get().setRollbackOnly();
		} else {
			this.unmanagedTransaction.ifPresent(LocalTransactionContext::setRollbackOnly);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#setSavepoint(java.lang.String)
	 */
	@Override
	public void setSavepoint(final String savepointName) {
		Optional<LocalTransactionContext> txContext = currentTxContext();
		if (txContext.isPresent()) {
			txContext.get().setSavepoint(savepointName);
		} else {
			this.unmanagedTransaction.ifPresent(ut -> ut.setSavepoint(savepointName));
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#releaseSavepoint(java.lang.String)
	 */
	@Override
	public void releaseSavepoint(final String savepointName) {
		Optional<LocalTransactionContext> txContext = currentTxContext();
		if (txContext.isPresent()) {
			txContext.get().releaseSavepoint(savepointName);
		} else {
			this.unmanagedTransaction.ifPresent(ut -> ut.releaseSavepoint(savepointName));
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#rollback(java.lang.String)
	 */
	@Override
	public void rollback(final String savepointName) {
		Optional<LocalTransactionContext> txContext = currentTxContext();
		if (txContext.isPresent()) {
			txContext.get().rollback(savepointName);
		} else {
			this.unmanagedTransaction.ifPresent(ut -> ut.rollback(savepointName));
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#getConnection()
	 */
	@Override
	public Connection getConnection() {
		Optional<LocalTransactionContext> txContext = currentTxContext();
		try {
			if (txContext.isPresent()) {
				return txContext.get().getConnection();
			} else {
				if (!this.unmanagedTransaction.isPresent()) {
					this.unmanagedTransaction = Optional
							.of(new LocalTransactionContext(this.connectionSupplier, this.sqlFilterManager));
				}
				return this.unmanagedTransaction.get().getConnection();
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
		Optional<LocalTransactionContext> txContext = currentTxContext();
		try {
			if (txContext.isPresent()) {
				return txContext.get().getConnection(alias);
			} else {
				if (!this.unmanagedTransaction.isPresent()) {
					this.unmanagedTransaction = Optional
							.of(new LocalTransactionContext(this.connectionSupplier, this.sqlFilterManager));
				}
				return this.unmanagedTransaction.get().getConnection(alias);
			}
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
		Optional<LocalTransactionContext> txContext = currentTxContext();
		try {
			if (txContext.isPresent()) {
				return txContext.get().getPreparedStatement(sqlContext);
			} else {
				if (!this.unmanagedTransaction.isPresent()) {
					this.unmanagedTransaction = Optional
							.of(new LocalTransactionContext(this.connectionSupplier, this.sqlFilterManager));
				}
				return this.unmanagedTransaction.get().getPreparedStatement(sqlContext);
			}
		} catch (SQLException ex) {
			ex.printStackTrace();
			return null;
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
		Optional<LocalTransactionContext> txContext = currentTxContext();
		try {
			if (txContext.isPresent()) {
				return txContext.get().getCallableStatement(sqlContext);
			} else {
				if (!this.unmanagedTransaction.isPresent()) {
					this.unmanagedTransaction = Optional
							.of(new LocalTransactionContext(this.connectionSupplier, this.sqlFilterManager));
				}
				return this.unmanagedTransaction.get().getCallableStatement(sqlContext);
			}
		} catch (SQLException ex) {
			ex.printStackTrace();
			return null;
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
	 *
	 * @param supplier SQLサプライヤ
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	private <R> R requiredInternal(final SQLSupplier<R> supplier) {
		if (currentTxContext().isPresent()) {
			return supplier.get();
		} else {
			return runInNewTx(supplier);
		}
	}

	/**
	 * requiresNewメソッドの内部実装
	 *
	 * @param supplier SQLサプライヤ
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	private <R> R requiresNewInternal(final SQLSupplier<R> supplier) {
		return runInNewTx(supplier);
	}

	/**
	 * notSupportedメソッドの内部実装
	 *
	 * @param supplier SQLサプライヤ
	 * @return 実行結果
	 */
	private <R> R notSupportedInternal(final SQLSupplier<R> supplier) {
		Optional<LocalTransactionContext> txContext = currentTxContext();
		if (txContext.isPresent()) {
			// トランザクションをサスペンド
			LocalTransactionContext txContextValue = this.txCtxStack.pop();
			try {
				return supplier.get();
			} finally {
				//戻す
				this.txCtxStack.push(txContextValue);
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
	 */
	private <R> R runInNewTx(final SQLSupplier<R> supplier) {
		try (LocalTransactionContext txContext = new LocalTransactionContext(this.connectionSupplier,
				this.sqlFilterManager)) {
			this.txCtxStack.push(txContext);
			try {
				return supplier.get();
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
					this.txCtxStack.pop();
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
		Optional<LocalTransactionContext> txContext = currentTxContext();
		if (txContext.isPresent()) {
			this.txCtxStack.forEach((elem) -> elem.close());
			this.txCtxStack.clear();
		} else {
			this.unmanagedTransaction.ifPresent(LocalTransactionContext::close);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#commit()
	 */
	@Override
	public void commit() {
		Optional<LocalTransactionContext> txContext = currentTxContext();
		if (txContext.isPresent()) {
			txContext.get().commit();
		} else {
			this.unmanagedTransaction.ifPresent(LocalTransactionContext::commit);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#rollback()
	 */
	@Override
	public void rollback() {
		Optional<LocalTransactionContext> txContext = currentTxContext();
		if (txContext.isPresent()) {
			txContext.get().rollback();
		} else {
			this.unmanagedTransaction.ifPresent(LocalTransactionContext::rollback);
		}
	}

}