package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * Entity Update時パラメータ設定後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterSetEntityUpdateParameterEvent extends ExecutionEvent {
	/** エンティティオブジェクト. */
	private final Object entity;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param entity エンティティオブジェクト
	 */
	public AfterSetEntityUpdateParameterEvent(final ExecutionContext executionContext, final Object entity) {
		super(executionContext);
		this.entity = entity;
	}

	/**
	 * エンティティオブジェクトの取得.
	 *
	 * @return エンティティオブジェクト
	 */
	public Object getEntity() {
		return entity;
	}

}
