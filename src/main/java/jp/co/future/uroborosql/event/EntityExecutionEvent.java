/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * Entity操作イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public abstract class EntityExecutionEvent extends ExecutionEvent {
	/** Entity. */
	private final Object entity;

	/** Entity Type. */
	private final Class<?> entityType;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param entity Entity
	 * @param entityType EntityType
	 */
	public EntityExecutionEvent(final ExecutionContext executionContext, final Object entity,
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
	 * EntityType の取得.
	 * @return entityType
	 * @return
	 */
	public Class<?> getEntityType() {
		return entityType;
	}
}
