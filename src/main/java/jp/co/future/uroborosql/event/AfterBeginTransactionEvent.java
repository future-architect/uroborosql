/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.Connection;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;

public class AfterBeginTransactionEvent extends TransactionEvent {
	/** ConnectionContext. */
	private final ConnectionContext connectionContext;

	/**
	 * コンストラクタ.
	 *
	 * @param connection Connection
	 * @param sqlConfig SqlConfig
	 * @param connectionContext ConnectionContext
	 */
	public AfterBeginTransactionEvent(final Connection connection, final SqlConfig sqlConfig,
			final ConnectionContext connectionContext) {
		super(connection, sqlConfig);
		this.connectionContext = connectionContext;
	}

	/**
	 * ConnectionContextの取得.
	 * @return ConnectionContext
	 */
	public ConnectionContext getConnectionContext() {
		return connectionContext;
	}

}
