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
import java.util.Deque;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedDeque;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * ローカルトランザクションマネージャ
 *
 * @author ota
 */
public class LocalTransactionManager implements TransactionManager {
	/** SQL設定クラス */
	private final SqlConfig sqlConfig;

	/** トランザクションコンテキストのスタック */
	private final Deque<LocalTransactionContext> txCtxStack = new ConcurrentLinkedDeque<>();

	/** トランザクション管理外の接続に利用する便宜上のトランザクション */
	private Optional<LocalTransactionContext> unmanagedTransaction = Optional.empty();

	/** トランザクション内での更新を強制するかどうか */
	private final boolean updatable;

	/** DB接続情報. ConnectionSupplierで指定したデフォルトの接続情報を使用する場合は<code>null</code>を指定する */
	private final ConnectionContext connectionContext;

	/**
	 * コンストラクタ
	 *
	 * @param sqlConfig SQL設定クラス
	 * @param connectionContext DB接続情報
	 */
	public LocalTransactionManager(final SqlConfig sqlConfig, final ConnectionContext connectionContext) {
		this.sqlConfig = sqlConfig;
		this.updatable = !sqlConfig.getSqlAgentProvider().isForceUpdateWithinTransaction();
		this.connectionContext = connectionContext;
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
		var txContext = currentTxContext();
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
		var txContext = currentTxContext();
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
		var txContext = currentTxContext();
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
		var txContext = currentTxContext();
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
		var txContext = currentTxContext();
		try {
			if (txContext.isPresent()) {
				return txContext.get().getConnection();
			} else {
				if (!this.unmanagedTransaction.isPresent()) {
					this.unmanagedTransaction = Optional
							.of(new LocalTransactionContext(this.sqlConfig, this.updatable, this.connectionContext));
				}
				return this.unmanagedTransaction.get().getConnection();
			}
		} catch (SQLException ex) {
			ex.printStackTrace();
			return null;
		}
	}

	/**
	 * ステートメント初期化。
	 *
	 * @param executionContext ExecutionContext
	 * @return PreparedStatement
	 * @throws SQLException SQL例外
	 */
	public PreparedStatement getPreparedStatement(final ExecutionContext executionContext) throws SQLException {
		var txContext = currentTxContext();
		if (txContext.isPresent()) {
			return txContext.get().getPreparedStatement(executionContext);
		} else {
			if (!this.unmanagedTransaction.isPresent()) {
				this.unmanagedTransaction = Optional
						.of(new LocalTransactionContext(this.sqlConfig, this.updatable, this.connectionContext));
			}
			return this.unmanagedTransaction.get().getPreparedStatement(executionContext);
		}
	}

	/**
	 * Callableステートメント初期化
	 *
	 * @param executionContext ExecutionContext
	 * @return CallableStatement
	 * @throws SQLException SQL例外
	 */
	public CallableStatement getCallableStatement(final ExecutionContext executionContext) throws SQLException {
		var txContext = currentTxContext();
		if (txContext.isPresent()) {
			return txContext.get().getCallableStatement(executionContext);
		} else {
			if (!this.unmanagedTransaction.isPresent()) {
				this.unmanagedTransaction = Optional
						.of(new LocalTransactionContext(this.sqlConfig, this.updatable, this.connectionContext));
			}
			return this.unmanagedTransaction.get().getCallableStatement(executionContext);
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
		var txContext = currentTxContext();
		if (txContext.isPresent()) {
			// トランザクションをサスペンド
			var txContextValue = this.txCtxStack.pop();
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
		try (var txContext = new LocalTransactionContext(this.sqlConfig, true,
				this.connectionContext)) {
			this.txCtxStack.push(txContext);
			try {
				return supplier.get();
			} catch (Throwable th) {
				txContext.setRollbackOnly();
				throw th;
			} finally {
				try {
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
		var txContext = currentTxContext();
		if (txContext.isPresent()) {
			this.txCtxStack.forEach(LocalTransactionContext::close);
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
		var txContext = currentTxContext();
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
		var txContext = currentTxContext();
		if (txContext.isPresent()) {
			txContext.get().rollback();
		} else {
			this.unmanagedTransaction.ifPresent(LocalTransactionContext::rollback);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#savepointScope(jp.co.future.uroborosql.tx.SQLSupplier)
	 */
	@Override
	public <R> R savepointScope(final SQLSupplier<R> supplier) {
		var savepointName = UUID.randomUUID().toString();
		setSavepoint(savepointName);
		try {
			return supplier.get();
		} catch (Throwable th) {
			rollback(savepointName);
			throw th;
		} finally {
			releaseSavepoint(savepointName);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#savepointScope(jp.co.future.uroborosql.tx.SQLRunnable)
	 */
	@Override
	public void savepointScope(final SQLRunnable runnable) {
		savepointScope(toSupplier(runnable));
	}

}