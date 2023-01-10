/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
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
