/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.util.EventObject;
import java.util.Optional;

import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.tx.LocalTransactionContext;

/**
 * トランザクション関連イベント.
 *
 * @author yanagihara
 */
public class TransactionalEvent extends EventObject {

	private final Optional<ConnectionContext> connectionContext;

	private TransactionalEvent(final LocalTransactionContext txContext,
			final Optional<ConnectionContext> connectionContext) {
		super(txContext);
		this.connectionContext = connectionContext;
	}

	/**
	 * {@link LocalTransactionContext}を取得します.
	 *
	 * @return {@link LocalTransactionContext}
	 */
	public LocalTransactionContext getTxContext() {
		return (LocalTransactionContext) getSource();
	}

	/**
	 * {@link ConnectionContext}を取得します.
	 *
	 * @return {@literal Optional<ConnectionContext>}
	 */
	public Optional<ConnectionContext> getConnectionContext() {
		return this.connectionContext;
	}

	/**
	 * トランザクション開始前イベント.
	 */
	public static class BeforeTransactionEvent extends TransactionalEvent {

		/**
		 * コンストラクタ.
		 *
		 * @param txContext {@link LocalTransactionContext}
		 * @param connectionContext {@link Optional}でラップされた{@link ConnectionContext}
		 */
		public BeforeTransactionEvent(final LocalTransactionContext txContext,
				final Optional<ConnectionContext> connectionContext) {
			super(txContext, connectionContext);
		}
	}

	/**
	 * トランザクション終了後イベント.
	 */
	public static class AfterTransactionEvent extends TransactionalEvent {

		/**
		 * コンストラクタ.
		 *
		 * @param txContext {@link LocalTransactionContext}
		 * @param connectionContext {@link Optional}でラップされた{@link ConnectionContext}
		 */
		public AfterTransactionEvent(final LocalTransactionContext txContext,
				final Optional<ConnectionContext> connectionContext) {
			super(txContext, connectionContext);
		}
	}

	/**
	 * コミット実行前イベント.
	 */
	public static class BeforeCommitEvent extends TransactionalEvent {

		/**
		 * コンストラクタ.
		 *
		 * @param txContext {@link LocalTransactionContext}
		 * @param connectionContext {@link Optional}でラップされた{@link ConnectionContext}
		 */
		public BeforeCommitEvent(final LocalTransactionContext txContext,
				final Optional<ConnectionContext> connectionContext) {
			super(txContext, connectionContext);
		}
	}

	/**
	 * コミット実行後イベント.
	 */
	public static class AfterCommitEvent extends TransactionalEvent {

		/**
		 * コンストラクタ.
		 *
		 * @param txContext {@link LocalTransactionContext}
		 * @param connectionContext {@link Optional}でラップされた{@link ConnectionContext}
		 */
		public AfterCommitEvent(final LocalTransactionContext txContext,
				final Optional<ConnectionContext> connectionContext) {
			super(txContext, connectionContext);
		}
	}

	/**
	 * ロールバック実行前イベント.
	 */
	public static class BeforeRollbackEvent extends TransactionalEvent {

		/**
		 * コンストラクタ.
		 *
		 * @param txContext {@link LocalTransactionContext}
		 * @param connectionContext {@link Optional}でラップされた{@link ConnectionContext}
		 */
		public BeforeRollbackEvent(final LocalTransactionContext txContext,
				final Optional<ConnectionContext> connectionContext) {
			super(txContext, connectionContext);
		}
	}

	/**
	 * ロールバック実行後イベント.
	 */
	public static class AfterRollbackEvent extends TransactionalEvent {

		/**
		 * コンストラクタ.
		 *
		 * @param txContext {@link LocalTransactionContext}
		 * @param connectionContext {@link Optional}でラップされた{@link ConnectionContext}
		 */
		public AfterRollbackEvent(final LocalTransactionContext txContext,
				final Optional<ConnectionContext> connectionContext) {
			super(txContext, connectionContext);
		}
	}
}
