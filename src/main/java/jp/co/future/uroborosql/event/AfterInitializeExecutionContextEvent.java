package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * ExecutionContext初期化後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterInitializeExecutionContextEvent extends ExecutionEvent {
	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 */
	public AfterInitializeExecutionContextEvent(final ExecutionContext executionContext) {
		super(executionContext);
	}
}
