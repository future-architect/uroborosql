/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.Connection;

import jp.co.future.uroborosql.config.SqlConfig;

/**
 * コネクションロールバック後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterRollbackEvent extends TransactionEvent {
	/**
	 * コンストラクタ.
	 *
	 * @param connection Connection
	 * @param sqlConfig SqlConfig
	 */
	public AfterRollbackEvent(final Connection connection, final SqlConfig sqlConfig) {
		super(connection, sqlConfig);
	}

}
