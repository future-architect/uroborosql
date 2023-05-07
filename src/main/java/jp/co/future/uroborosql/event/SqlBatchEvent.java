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
 * SqlBatch実行後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class SqlBatchEvent extends ExecutionEvent {
	/** 実行結果. */
	private int[] counts;
	/** PreparedStatement. */
	private final PreparedStatement preparedStatement;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param counts 実行結果
	 * @param preparedStatement PreparedStatement
	 */
	public SqlBatchEvent(final ExecutionContext executionContext,
			final int[] counts,
			final PreparedStatement preparedStatement) {
		super(executionContext);
		this.counts = counts;
		this.preparedStatement = preparedStatement;
	}

	/**
	 * 実行結果の取得.
	 * @return 実行結果
	 */
	public int[] getCounts() {
		return counts;
	}

	/**
	 * 実行結果の設定.
	 * @param counts 実行結果
	 */
	public void setCounts(final int[] counts) {
		this.counts = counts;
	}

	/**
	 * PreparedStatementの取得.
	 * @return PreparedStatement
	 */
	public PreparedStatement getPreparedStatement() {
		return preparedStatement;
	}

}