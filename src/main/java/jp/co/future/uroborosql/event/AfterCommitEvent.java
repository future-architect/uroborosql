package jp.co.future.uroborosql.event;

import java.sql.Connection;

import jp.co.future.uroborosql.config.SqlConfig;

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
	 * @param connection Connection
	 * @param sqlConfig SqlConfig
	 */
	public AfterCommitEvent(final Connection connection, final SqlConfig sqlConfig) {
		super(connection, sqlConfig);
	}

}
