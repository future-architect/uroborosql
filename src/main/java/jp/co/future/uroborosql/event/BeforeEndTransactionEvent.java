package jp.co.future.uroborosql.event;

import java.sql.Connection;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;

/**
 * トランザクション終了前イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class BeforeEndTransactionEvent extends TransactionEvent {
	/** ConnectionContext. */
	private final ConnectionContext connectionContext;
	/** 実行結果. */
	private final Object result;

	/**
	 * コンストラクタ.
	 *
	 * @param connection Connection
	 * @param sqlConfig SqlConfig
	 * @param connectionContext ConnectionContext
	 * @param result 実行結果
	 */
	public BeforeEndTransactionEvent(final Connection connection,
			final SqlConfig sqlConfig,
			final ConnectionContext connectionContext,
			final Object result) {
		super(connection, sqlConfig);
		this.connectionContext = connectionContext;
		this.result = result;
	}

	/**
	 * ConnectionContextの取得.
	 * @return ConnectionContext
	 */
	public ConnectionContext getConnectionContext() {
		return connectionContext;
	}

	/**
	 * 実行結果の取得.
	 * @return 実行結果
	 */
	public Object getResult() {
		return result;
	}
}
