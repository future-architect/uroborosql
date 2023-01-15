/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.tx.TransactionContext;

/**
 * Transactionイベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public abstract class TransactionEvent extends AbstractEvent {
	/**
	 * コンストラクタ.
	 *
	 * @param transactionContext TransactionContext
	 */
	protected TransactionEvent(TransactionContext transactionContext) {
		super(transactionContext);
	}

	/**
	 * TransactionContextの取得.
	 * @return TransactionContext
	 */
	public TransactionContext getTransactionContext() {
		return (TransactionContext) getSource();
	}
}
