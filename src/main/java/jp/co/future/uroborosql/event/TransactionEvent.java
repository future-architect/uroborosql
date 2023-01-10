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
 * Transactionイベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public abstract class TransactionEvent extends AbstractEvent {
	/** SqlConfig. */
	private final SqlConfig sqlConfig;

	/**
	 * コンストラクタ.
	 *
	 * @param connection Connection
	 * @param sqlConfig SqlConfig
	 */
	protected TransactionEvent(final Connection connection, final SqlConfig sqlConfig) {
		super(connection);
		this.sqlConfig = sqlConfig;
	}

	/**
	 * Connectionの取得.
	 * @return Connection
	 */
	public Connection getConnection() {
		return (Connection) getSource();
	}

	/**
	 * SqlConfigの取得.
	 * @return SqlConfig
	 */
	public SqlConfig getSqlConfig() {
		return sqlConfig;
	}

}
