/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.CallableStatement;
import java.sql.Connection;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * Procedure実行後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class ProcedureEvent extends ExecutionEvent {
	/** 実行結果. 最初の結果がResultSetオブジェクトの場合はtrue。更新カウントであるか、または結果がない場合はfalse. */
	private final boolean result;

	/** Connection. */
	private final Connection connection;

	/** CallableStatement. */
	private final CallableStatement callableStatement;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param result 実行結果. 最初の結果がResultSetオブジェクトの場合はtrue。更新カウントであるか、または結果がない場合はfalse
	 * @param connection Connection
	 * @param callableStatement CallableStatement
	 */
	public ProcedureEvent(final ExecutionContext executionContext,
			final boolean result,
			final Connection connection,
			final CallableStatement callableStatement) {
		super(executionContext);
		this.result = result;
		this.connection = connection;
		this.callableStatement = callableStatement;
	}

	/**
	 * 実行結果の取得.
	 * @return 実行結果. 最初の結果がResultSetオブジェクトの場合はtrue。更新カウントであるか、または結果がない場合はfalse.
	 */
	public boolean isResult() {
		return result;
	}

	/**
	 * Connectionの取得.
	 * @return Connection
	 */
	public Connection getConnection() {
		return connection;
	}

	/**
	 * CallableStatementの取得.
	 * @return CallableStatement
	 */
	public CallableStatement getCallableStatement() {
		return callableStatement;
	}

}
