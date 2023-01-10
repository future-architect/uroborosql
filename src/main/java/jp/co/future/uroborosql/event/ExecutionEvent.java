package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * Executionイベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public abstract class ExecutionEvent extends AbstractEvent {
	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 */
	protected ExecutionEvent(final ExecutionContext executionContext) {
		super(executionContext);
	}

	/**
	 * ExecutionContextの取得.
	 * @return ExecutionContext
	 */
	public ExecutionContext getExecutionContext() {
		return (ExecutionContext) getSource();
	}
}
