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
import java.util.function.Supplier;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.event.AfterBeginTransactionEvent;
import jp.co.future.uroborosql.event.BeforeEndTransactionEvent;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.exception.UroborosqlTransactionException;

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
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(java.lang.Runnable)
	 */
	@Override
	public void required(final Runnable runnable) {
		requiredInternal(toSupplier(runnable));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#required(java.util.function.Supplier)
	 */
	@Override
	public <R> R required(final Supplier<R> supplier) {
		return requiredInternal(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(java.lang.Runnable)
	 */
	@Override
	public void requiresNew(final Runnable runnable) {
		requiresNewInternal(toSupplier(runnable));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#requiresNew(java.util.function.Supplier)
	 */
	@Override
	public <R> R requiresNew(final Supplier<R> supplier) {
		return requiresNewInternal(supplier);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(java.lang.Runnable)
	 */
	@Override
	public void notSupported(final Runnable runnable) {
		notSupportedInternal(toSupplier(runnable));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#notSupported(java.util.function.Supplier)
	 */
	@Override
	public <R> R notSupported(final Supplier<R> supplier) {
		return notSupportedInternal(supplier);
	}

	/**
	 * unmanagedTransactionも含め、現在有効なTransactionContextを取得する
	 *
	 * @param unmanagedCreate unmanagedTransactionが利用される場合に、unmanagedTransactionが未作成ならunmanagedTransactionを作成するかどうか。<code>true</code>なら作成する.
	 * @return unmanagedTransactionも含め、現在有効なTransactionContext
	 */
	private Optional<LocalTransactionContext> currentTxContext(final boolean unmanagedCreate) {
		return Optional.ofNullable(this.txCtxStack.peek()).or(() -> {
			if (unmanagedCreate && this.unmanagedTransaction.isEmpty()) {
				this.unmanagedTransaction = Optional
						.of(new LocalTransactionContext(this.sqlConfig, this.updatable, this.connectionContext));
			}
			return this.unmanagedTransaction;
		});
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#setRollbackOnly()
	 */
	@Override
	public void setRollbackOnly() {
		currentTxContext(false).ifPresent(LocalTransactionContext::setRollbackOnly);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#setSavepoint(java.lang.String)
	 */
	@Override
	public void setSavepoint(final String savepointName) {
		currentTxContext(false).ifPresent(txCtx -> txCtx.setSavepoint(savepointName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#releaseSavepoint(java.lang.String)
	 */
	@Override
	public void releaseSavepoint(final String savepointName) {
		currentTxContext(false).ifPresent(txCtx -> txCtx.releaseSavepoint(savepointName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#rollback(java.lang.String)
	 */
	@Override
	public void rollback(final String savepointName) {
		currentTxContext(false).ifPresent(txCtx -> txCtx.rollback(savepointName));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#getConnection()
	 */
	@Override
	public Connection getConnection() {
		try {
			return currentTxContext(true).orElseThrow().getConnection();
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
		return currentTxContext(true).orElseThrow().getPreparedStatement(executionContext);
	}

	/**
	 * Callableステートメント初期化
	 *
	 * @param executionContext ExecutionContext
	 * @return CallableStatement
	 * @throws SQLException SQL例外
	 */
	public CallableStatement getCallableStatement(final ExecutionContext executionContext) throws SQLException {
		return currentTxContext(true).orElseThrow().getCallableStatement(executionContext);
	}

	/**
	 * 処理ブロックの実行
	 *
	 * @param runnable 処理ブロック
	 * @return サプライヤ
	 */
	private Supplier<Void> toSupplier(final Runnable runnable) {
		return () -> {
			runnable.run();
			return null;
		};
	}

	/**
	 * requiredメソッドの内部実装
	 *
	 * @param supplier サプライヤ
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	private <R> R requiredInternal(final Supplier<R> supplier) {
		if (!this.txCtxStack.isEmpty()) {
			return supplier.get();
		} else {
			return runInNewTx(supplier);
		}
	}

	/**
	 * requiresNewメソッドの内部実装
	 *
	 * @param supplier サプライヤ
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	private <R> R requiresNewInternal(final Supplier<R> supplier) {
		return runInNewTx(supplier);
	}

	/**
	 * notSupportedメソッドの内部実装
	 *
	 * @param supplier サプライヤ
	 * @return 実行結果
	 */
	private <R> R notSupportedInternal(final Supplier<R> supplier) {
		if (!this.txCtxStack.isEmpty()) {
			// トランザクションをサスペンド
			var txCtx = this.txCtxStack.pop();
			try {
				return supplier.get();
			} finally {
				//戻す
				this.txCtxStack.push(txCtx);
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
	private <R> R runInNewTx(final Supplier<R> supplier) {
		try (var txCtx = new LocalTransactionContext(this.sqlConfig, true, this.connectionContext)) {
			this.txCtxStack.push(txCtx);
			try {
				// トランザクション開始後イベント発行
				if (this.sqlConfig.getEventListenerHolder().hasAfterBeginTransactionListener()) {
					var eventObj = new AfterBeginTransactionEvent(txCtx.getConnection(), this.sqlConfig,
							this.connectionContext);
					this.sqlConfig.getEventListenerHolder().getAfterBeginTransactionListeners()
							.forEach(listener -> listener.accept(eventObj));
				}
				R result = null;
				try {
					result = supplier.get();
					return result;
				} finally {
					// トランザクション終了前イベント発行
					if (this.sqlConfig.getEventListenerHolder().hasBeforeEndTransactionListener()) {
						var eventObj = new BeforeEndTransactionEvent(txCtx.getConnection(), this.sqlConfig,
								this.connectionContext, result);
						this.sqlConfig.getEventListenerHolder().getBeforeEndTransactionListeners()
								.forEach(listener -> listener.accept(eventObj));
					}
				}
			} catch (SQLException ex) {
				txCtx.setRollbackOnly();
				throw new UroborosqlTransactionException(ex);
			} catch (Throwable th) {
				txCtx.setRollbackOnly();
				throw th;
			}
		} finally {
			this.txCtxStack.pop();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#close()
	 */
	@Override
	public void close() {
		if (!this.txCtxStack.isEmpty()) {
			this.txCtxStack.forEach(LocalTransactionContext::close);
			this.txCtxStack.clear();
		}
		this.unmanagedTransaction.ifPresent(LocalTransactionContext::close);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#commit()
	 */
	@Override
	public void commit() {
		currentTxContext(false).ifPresent(LocalTransactionContext::commit);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#rollback()
	 */
	@Override
	public void rollback() {
		currentTxContext(false).ifPresent(LocalTransactionContext::rollback);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#savepointScope(java.util.function.Supplier)
	 */
	@Override
	public <R> R savepointScope(final Supplier<R> supplier) {
		return currentTxContext(false).map(txCtx -> {
			var savepointName = UUID.randomUUID().toString();
			txCtx.setSavepoint(savepointName);
			try {
				return supplier.get();
			} catch (Throwable th) {
				txCtx.rollback(savepointName);
				throw th;
			} finally {
				txCtx.releaseSavepoint(savepointName);
			}
		}).orElse(null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#savepointScope(java.lang.Runnable)
	 */
	@Override
	public void savepointScope(final Runnable runnable) {
		savepointScope(toSupplier(runnable));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#autoCommitScope(java.util.function.Supplier)
	 */
	@Override
	public <R> R autoCommitScope(final Supplier<R> supplier) {
		try (var txCtx = new LocalTransactionContext(this.sqlConfig, true, this.connectionContext)) {
			this.txCtxStack.push(txCtx);

			var conn = txCtx.getConnection();
			var preserveAutoCommitState = conn.getAutoCommit();
			try {
				// autoCommit=trueに設定する
				if (!preserveAutoCommitState) {
					conn.setAutoCommit(true);
				}
				return supplier.get();
			} catch (Throwable th) {
				txCtx.setRollbackOnly();
				throw th;
			} finally {
				if (!preserveAutoCommitState) {
					conn.setAutoCommit(preserveAutoCommitState);
				}
			}
		} catch (SQLException se) {
			throw new UroborosqlSQLException(se);
		} finally {
			this.txCtxStack.pop().close();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.tx.TransactionManager#autoCommitScope(java.lang.Runnable)
	 */
	@Override
	public void autoCommitScope(final Runnable runnable) {
		autoCommitScope(toSupplier(runnable));
	}

}