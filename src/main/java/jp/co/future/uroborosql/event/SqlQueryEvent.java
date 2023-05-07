/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.PreparedStatement;
import java.sql.ResultSet;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * SqlQuery実行後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class SqlQueryEvent extends ExecutionEvent {
	/** ResultSet. */
	private ResultSet resultSet;
	/** PreparedStatement. */
	private final PreparedStatement preparedStatement;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param resultSet ResultSet
	 * @param preparedStatement PreparedStatement
	 */
	public SqlQueryEvent(final ExecutionContext executionContext,
			final ResultSet resultSet,
			final PreparedStatement preparedStatement) {
		super(executionContext);
		this.resultSet = resultSet;
		this.preparedStatement = preparedStatement;
	}

	/**
	 * ResultSetの取得.
	 * @return ResultSet
	 */
	public ResultSet getResultSet() {
		return resultSet;
	}

	/**
	 * ResultSetの設定.
	 * @param resultSet ResultSet
	 */
	public void setResultSet(final ResultSet resultSet) {
		this.resultSet = resultSet;
	}

	/**
	 * PreparedStatementの取得.
	 * @return PreparedStatement
	 */
	public PreparedStatement getPreparedStatement() {
		return preparedStatement;
	}

}