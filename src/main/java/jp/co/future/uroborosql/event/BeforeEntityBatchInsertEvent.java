/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * EntityBatchInsert前イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class BeforeEntityBatchInsertEvent extends ExecutionEvent {
	/**
	 * Entity.<br>
	 * batch insertでは大量のEntityが処理されることになるため、イベントインスタンスを都度設定するのではなく、１つのイベントインスタンスを使いまわすように変更可能にしている.
	 */
	private Object entity;

	/** Entity Type. */
	private final Class<?> entityType;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param entity Entity
	 * @param entityType EntityType
	 */
	public BeforeEntityBatchInsertEvent(final ExecutionContext executionContext, final Object entity,
			final Class<?> entityType) {
		super(executionContext);
		this.entity = entity;
		this.entityType = entityType;
	}

	/**
	 * Entity の取得.
	 * @return entity
	 */
	public Object getEntity() {
		return entity;
	}

	/**
	 * Entity の設定.
	 * @param entity
	 * @deprecated use inner logic only
	 */
	@Deprecated
	public void setEntity(final Object entity) {
		this.entity = entity;
	}

	/**
	 * EntityType の取得.
	 * @return entityType
	 * @return
	 */
	public Class<?> getEntityType() {
		return entityType;
	}
}
