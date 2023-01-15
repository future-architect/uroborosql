/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.tx.TransactionContext;

/**
 * コネクションコミット後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterCommitEvent extends TransactionEvent {

	/**
	 * コンストラクタ.
	 *
	 * @param transactionContext TransactionContext
	 */
	public AfterCommitEvent(TransactionContext transactionContext) {
		super(transactionContext);
	}
}
