/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.Connection;
import java.sql.PreparedStatement;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * PreparedStatement作成後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterCreatePreparedStatementEvent extends ExecutionEvent {
	/** Connection. */
	private final Connection connection;

	/** PreparedStatement. */
	private PreparedStatement preparedStatement;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param connection Connection
	 * @param preparedStatement PreparedStatement
	 */
	public AfterCreatePreparedStatementEvent(final ExecutionContext executionContext,
			final Connection connection,
			final PreparedStatement preparedStatement) {
		super(executionContext);
		this.connection = connection;
		this.preparedStatement = preparedStatement;
	}

	/**
	 * Connectionの取得.
	 * @return Connection
	 */
	public Connection getConnection() {
		return connection;
	}

	/**
	 * PreparedStatementの取得.
	 * @return PreparedStatement
	 */
	public PreparedStatement getPreparedStatement() {
		return preparedStatement;
	}

	/**
	 * PreparedStatementの設定.
	 * @param preparedStatement PreparedStatement
	 */
	public void setPreparedStatement(final PreparedStatement preparedStatement) {
		this.preparedStatement = preparedStatement;
	}
}
