/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.EventObject;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * 実行結果イベント.
 *
 * @author yanagihara
 */
public class ResultEvent extends EventObject {

	private final PreparedStatement preparedStatement;

	private ResultEvent(final ExecutionContext executionContext, final PreparedStatement preparedStatement) {
		super(executionContext);
		this.preparedStatement = preparedStatement;
	}

	/**
	 * {@link ExecutionContext}を取得します.
	 *
	 * @return {@link ExecutionContext}
	 */
	public ExecutionContext getExecutionContext() {
		return (ExecutionContext) getSource();
	}

	protected PreparedStatement getStatement() {
		return this.preparedStatement;
	}

	/**
	 * バッチ実行結果イベント.
	 */
	public static class BatchResultEvent extends ResultEvent {

		private final int[] result;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param preparedStatement {@link PreparedStatement}
		 * @param result 更新件数
		 */
		public BatchResultEvent(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
				final int[] result) {
			super(executionContext, preparedStatement);
			this.result = result;
		}

		/**
		 * {@link PreparedStatement}を取得します.
		 *
		 * @return {@link PreparedStatement}
		 */
		public PreparedStatement getPreparedStatement() {
			return getStatement();
		}

		/**
		 * 更新件数を取得します.
		 *
		 * @return 更新件数
		 */
		public int[] getResult() {
			return this.result;
		}
	}

	/**
	 * プロシージャ実行結果イベント.
	 */
	public static class ProcedureResultEvent extends ResultEvent {

		private final boolean result;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param callableStatement {@link CallableStatement}
		 * @param result 実行結果
		 */
		public ProcedureResultEvent(final ExecutionContext executionContext, final CallableStatement callableStatement,
				final boolean result) {
			super(executionContext, callableStatement);
			this.result = result;
		}

		/**
		 * {@link CallableStatement}を取得します.
		 *
		 * @return {@link CallableStatement}
		 */
		public CallableStatement getCallableStatement() {
			return (CallableStatement) getStatement();
		}

		/**
		 * 実行結果を取得します.
		 *
		 * @return true:結果が{@link ResultSet}の場合 / false:結果が更新件数であるか、結果なしの場合
		 */
		public boolean getResult() {
			return this.result;
		}
	}

	/**
	 * クエリ実行結果イベント.
	 */
	public static class QueryResultEvent extends ResultEvent {

		private final ResultSet resultSet;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param preparedStatement {@link PreparedStatement}
		 * @param resultSet {@link ResultSet}
		 */
		public QueryResultEvent(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
				final ResultSet resultSet) {
			super(executionContext, preparedStatement);
			this.resultSet = resultSet;
		}

		/**
		 * {@link PreparedStatement}を取得します.
		 *
		 * @return {@link PreparedStatement}
		 */
		public PreparedStatement getPreparedStatement() {
			return getStatement();
		}

		/**
		 * {@link ResultSet}を取得します.
		 *
		 * @return {@link ResultSet}
		 */
		public ResultSet getResultSet() {
			return this.resultSet;
		}
	}

	/**
	 * 更新実行結果イベント.
	 */
	public static class UpdateResultEvent extends ResultEvent {

		private final int result;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param preparedStatement {@link PreparedStatement}
		 * @param result 更新件数
		 */
		public UpdateResultEvent(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
				final int result) {
			super(executionContext, preparedStatement);
			this.result = result;
		}

		/**
		 * {@link PreparedStatement}を取得します.
		 *
		 * @return {@link PreparedStatement}
		 */
		public PreparedStatement getPreparedStatement() {
			return getStatement();
		}

		/**
		 * 更新件数を取得します.
		 *
		 * @return 更新件数
		 */
		public int getResult() {
			return this.result;
		}
	}
}
