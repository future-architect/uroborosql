/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.tx.TransactionContext;

/**
 * トランザクション開始後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterBeginTransactionEvent extends TransactionEvent {
	/** 新規トランザクションかどうか */
	private final boolean isRequiredNew;
	/** トランザクション階層 */
	private final int transactionLevel;

	/**
	 * コンストラクタ.
	 *
	 * @param transactionContext TransactionContext
	 * @param isRequiredNew 新規トランザクションかどうか
	 * @param transactionLevel トランザクション階層
	 */
	public AfterBeginTransactionEvent(TransactionContext transactionContext,
			final boolean isRequiredNew,
			final int transactionLevel) {
		super(transactionContext);
		this.isRequiredNew = isRequiredNew;
		this.transactionLevel = transactionLevel;
	}

	/**
	 * 新規トランザクションかどうかの取得.
	 * @return 新規トランザクションかどうか
	 */
	public boolean isRequiredNew() {
		return isRequiredNew;
	}

	/**
	 * トランザクション階層の取得.
	 * @return トランザクション階層
	 */
	public int getTransactionLevel() {
		return transactionLevel;
	}

}
