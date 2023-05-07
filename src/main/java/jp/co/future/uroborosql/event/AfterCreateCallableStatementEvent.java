/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.CallableStatement;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * CallableStatement作成後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterCreateCallableStatementEvent extends ExecutionEvent {
	/** CallableStatement. */
	private CallableStatement callableStatement;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param callableStatement CallableStatement
	 */
	public AfterCreateCallableStatementEvent(final ExecutionContext executionContext,
			final CallableStatement callableStatement) {
		super(executionContext);
		this.callableStatement = callableStatement;
	}

	/**
	 * CallableStatementの取得.
	 * @return CallableStatement
	 */
	public CallableStatement getCallableStatement() {
		return callableStatement;
	}

	/**
	 * CallableStatementの設定.
	 * @param callableStatement CallableStatement
	 */
	public void setCallableStatement(final CallableStatement callableStatement) {
		this.callableStatement = callableStatement;
	}

}