/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.Connection;

import jp.co.future.uroborosql.tx.TransactionContext;

/**
 * コネクションロールバック後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterRollbackEvent extends TransactionEvent {
	/** Connection. */
	private final Connection connection;

	/**
	 * コンストラクタ.
	 *
	 * @param transactionContext TransactionContext
	 * @param connection Connection
	 */
	public AfterRollbackEvent(final TransactionContext transactionContext, final Connection connection) {
		super(transactionContext);
		this.connection = connection;
	}

	/**
	 * Connectionの取得.
	 * @return Connection
	 */
	public Connection getConnection() {
		return connection;
	}

}
