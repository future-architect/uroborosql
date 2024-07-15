/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * EntityQuery前イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class BeforeEntityQueryEvent extends EntityExecutionEvent {

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param entity Entity
	 * @param entityType EntityType
	 */
	public BeforeEntityQueryEvent(final ExecutionContext executionContext, final Object entity,
			final Class<?> entityType) {
		super(executionContext, entity, entityType);
	}

}
