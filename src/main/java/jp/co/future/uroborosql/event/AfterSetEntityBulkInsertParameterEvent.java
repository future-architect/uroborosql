package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * Entity BulkInsert時パラメータ設定後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterSetEntityBulkInsertParameterEvent extends ExecutionEvent {
	/** エンティティオブジェクト. */
	private final Object entity;

	/** エンティティのインデックス. */
	private final int entityIndex;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param entity エンティティオブジェクト
	 * @param entityIndex エンティティのインデックス
	 */
	public AfterSetEntityBulkInsertParameterEvent(final ExecutionContext executionContext,
			final Object entity,
			final int entityIndex) {
		super(executionContext);
		this.entity = entity;
		this.entityIndex = entityIndex;
	}

	/**
	 * エンティティオブジェクトの取得.
	 *
	 * @return エンティティオブジェクト
	 */
	public Object getEntity() {
		return entity;
	}

	/**
	 * エンティティのインデックスの取得.
	 * @return エンティティのインデックス
	 */
	public int getEntityIndex() {
		return entityIndex;
	}

}
