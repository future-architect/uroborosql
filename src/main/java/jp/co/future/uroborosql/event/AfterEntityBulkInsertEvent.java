/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.util.List;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * EntityBulkInsert後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterEntityBulkInsertEvent extends ExecutionEvent {
	/** Entity List. */
	private final List<?> entityList;

	/** Entity Type. */
	private final Class<?> entityType;

	/** Bulk frame count. */
	private final int frameCount;

	/** 更新件数. */
	private int count;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param entityList Entity List
	 * @param entityType EntityType
	 * @param frameCount Bulk frame count
	 * @param count 更新件数
	 */
	public AfterEntityBulkInsertEvent(final ExecutionContext executionContext, final List<?> entityList,
			final Class<?> entityType, final int frameCount, final int count) {
		super(executionContext);
		this.entityList = entityList;
		this.entityType = entityType;
		this.frameCount = frameCount;
		this.count = count;
	}

	/**
	 * Entity Listの取得.
	 * @return Entity List
	 */
	public List<?> getEntityList() {
		return entityList;
	}

	/**
	 * EntityType の取得.
	 * @return entityType
	 * @return
	 */
	public Class<?> getEntityType() {
		return entityType;
	}

	/**
	 * Bulk frame countの取得.
	 * @return Bulk frame count
	 */
	public int getFrameCount() {
		return frameCount;
	}

	/**
	 * 更新件数の取得.
	 * @return 更新件数
	 */
	public int getCount() {
		return count;
	}

	/**
	 * 更新件数の設定.
	 * @param count 更新件数
	 */
	public void setCount(final int count) {
		this.count = count;
	}

}
