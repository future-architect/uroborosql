/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.PreparedStatement;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * SqlUpdate実行後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class SqlUpdateEvent extends ExecutionEvent {
	/** 実行結果. */
	private int count;
	/** PreparedStatement. */
	private final PreparedStatement preparedStatement;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param count 実行結果
	 * @param preparedStatement PreparedStatement
	 */
	public SqlUpdateEvent(final ExecutionContext executionContext,
			final int count,
			final PreparedStatement preparedStatement) {
		super(executionContext);
		this.count = count;
		this.preparedStatement = preparedStatement;
	}

	/**
	 * 実行結果の取得.
	 * @return 実行結果
	 */
	public int getCount() {
		return count;
	}

	/**
	 * 実行結果の設定.
	 * @param count 実行結果
	 */
	public void setCount(final int count) {
		this.count = count;
	}

	/**
	 * PreparedStatementの取得.
	 * @return PreparedStatement
	 */
	public PreparedStatement getPreparedStatement() {
		return preparedStatement;
	}

}
