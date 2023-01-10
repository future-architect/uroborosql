package jp.co.future.uroborosql.event;

import java.sql.Connection;

import jp.co.future.uroborosql.config.SqlConfig;

public class BeforeCommitEvent extends TransactionEvent {
	/**
	 * コンストラクタ.
	 *
	 * @param connection Connection
	 * @param sqlConfig SqlConfig
	 */
	public BeforeCommitEvent(final Connection connection, final SqlConfig sqlConfig) {
		super(connection, sqlConfig);
	}
}
