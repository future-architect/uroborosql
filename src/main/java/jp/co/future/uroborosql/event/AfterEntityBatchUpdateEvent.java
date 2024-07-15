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
 * EntityBatchUpdate後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterEntityBatchUpdateEvent extends ExecutionEvent {
	/** Entity List. */
	private final List<?> entityList;

	/** Entity Type. */
	private final Class<?> entityType;

	/** 更新件数配列. */
	private int[] counts;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param entityList Entity List
	 * @param entityType EntityType
	 * @param counts 更新件数配列
	 */
	public AfterEntityBatchUpdateEvent(final ExecutionContext executionContext, final List<?> entityList,
			final Class<?> entityType, final int[] counts) {
		super(executionContext);
		this.entityList = entityList;
		this.entityType = entityType;
		this.counts = counts;
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
	 * 更新件数配列の取得.
	 * @return 更新件数配列
	 */
	public int[] getCounts() {
		return counts;
	}

	/**
	 * 更新件数配列の設定.
	 * @param counts 更新件数配列
	 */
	public void setCounts(final int[] counts) {
		this.counts = counts;
	}
}
