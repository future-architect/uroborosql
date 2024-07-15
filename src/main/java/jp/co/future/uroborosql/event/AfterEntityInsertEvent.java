/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * EntityInsert後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterEntityInsertEvent extends EntityExecutionEvent {
	/** 登録件数. */
	private int count;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param entity Entity
	 * @param entityType EntityType
	 * @param count 登録件数
	 */
	public AfterEntityInsertEvent(final ExecutionContext executionContext, final Object entity,
			final Class<?> entityType, final int count) {
		super(executionContext, entity, entityType);
		this.count = count;
	}

	/**
	 * 登録件数の取得
	 * @return 登録件数
	 */
	public int getCount() {
		return count;
	}

	/**
	 * 登録件数の設定
	 * @param count 登録件数
	 */
	public void setCount(final int count) {
		this.count = count;
	}

}
