package jp.co.future.uroborosql.event;

import java.sql.PreparedStatement;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * PreparedStatement作成後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterCreatePreparedStatementEvent extends ExecutionEvent {
	/** PreparedStatement. */
	private PreparedStatement preparedStatement;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param preparedStatement PreparedStatement
	 */
	public AfterCreatePreparedStatementEvent(final ExecutionContext executionContext,
			final PreparedStatement preparedStatement) {
		super(executionContext);
		this.preparedStatement = preparedStatement;
	}

	/**
	 * PreparedStatementの取得.
	 * @return PreparedStatement
	 */
	public PreparedStatement getPreparedStatement() {
		return preparedStatement;
	}

	/**
	 * PreparedStatementの設定.
	 * @param preparedStatement PreparedStatement
	 */
	public void setPreparedStatement(final PreparedStatement preparedStatement) {
		this.preparedStatement = preparedStatement;
	}
}
