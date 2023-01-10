package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * DAO Update時パラメータ設定後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterSetDaoUpdateParameterEvent extends ExecutionEvent {
	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 */
	public AfterSetDaoUpdateParameterEvent(final ExecutionContext executionContext) {
		super(executionContext);
	}
}
