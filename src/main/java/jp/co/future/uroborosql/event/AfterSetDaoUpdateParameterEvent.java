/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * DAO Update時パラメータ設定後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterSetDaoUpdateParameterEvent extends ExecutionEvent {
	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 */
	public AfterSetDaoUpdateParameterEvent(final ExecutionContext executionContext) {
		super(executionContext);
	}
}
