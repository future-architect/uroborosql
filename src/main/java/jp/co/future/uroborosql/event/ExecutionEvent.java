/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * Executionイベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public abstract class ExecutionEvent extends AbstractEvent {
	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 */
	protected ExecutionEvent(final ExecutionContext executionContext) {
		super(executionContext);
	}

	/**
	 * ExecutionContextの取得.
	 * @return ExecutionContext
	 */
	public ExecutionContext getExecutionContext() {
		return (ExecutionContext) getSource();
	}

}
