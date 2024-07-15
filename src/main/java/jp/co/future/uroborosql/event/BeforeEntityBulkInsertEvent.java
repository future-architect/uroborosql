/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * EntityBulkInsert前イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class BeforeEntityBulkInsertEvent extends ExecutionEvent {
	/**
	 * Entity.<br>
	 * bulk insertでは大量のEntityが処理されることになるため、イベントインスタンスを都度設定するのではなく、１つのイベントインスタンスを使いまわすように変更可能にしている.
	 */
	private Object entity;

	/** Entity Type. */
	private final Class<?> entityType;

	/**
	 * Bulk frame count.<br>
	 * bulk insertでは大量のEntityが処理されることになるため、イベントインスタンスを都度設定するのではなく、１つのイベントインスタンスを使いまわすように変更可能にしている.
	 */
	private int frameCount;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param entity Entity
	 * @param entityType EntityType
	 * @param frameCount Bulk frame count
	 */
	public BeforeEntityBulkInsertEvent(final ExecutionContext executionContext, final Object entity,
			final Class<?> entityType, final int frameCount) {
		super(executionContext);
		this.entity = entity;
		this.entityType = entityType;
		this.frameCount = frameCount;
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
	 * @param entity Entity
	 * @deprecated use inner logic only
	 */
	@Deprecated
	public void setEntity(final Object entity) {
		this.entity = entity;
	}

	/**
	 * EntityType の取得.
	 * @return entityType
	 */
	public Class<?> getEntityType() {
		return entityType;
	}

	/**
	 * Bulk frame count の取得.
	 * @return Bulk frame count
	 */
	public int getFrameCount() {
		return frameCount;
	}

	/**
	 * Bulk frame count の設定.
	 * @param frameCount Bulk frame count
	 * @deprecated use inner logic only
	 */
	@Deprecated
	public void setFrameCount(final int frameCount) {
		this.frameCount = frameCount;
	}

}
