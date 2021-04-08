/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.util.EventObject;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * SQL構築イベント.
 *
 * @author yanagihara
 */
public class SqlEvent extends EventObject {

	private SqlEvent(final Object executionContext) {
		super(executionContext);
	}

	/**
	 * {@link ExecutionContext}を取得します.
	 *
	 * @return {@link ExecutionContext}
	 */
	public ExecutionContext getExecutionContext() {
		return (ExecutionContext) getSource();
	}

	/**
	 * SQL変換イベント.
	 */
	public static class TransformSqlEvent extends SqlEvent {

		private final String originalSql;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param originalSql 変換前SQL
		 */
		public TransformSqlEvent(final ExecutionContext executionContext, final String originalSql) {
			super(executionContext);
			this.originalSql = originalSql;
		}

		/**
		 * 変換前SQLを取得します.
		 *
		 * @return 変換前SQL
		 */
		public String getOriginalSql() {
			return this.originalSql;
		}
	}

	/**
	 * PreparedStatement作成イベント.
	 */
	public static class PreparedStatementEvent extends SqlEvent {

		private final PreparedStatement preparedStatement;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param preparedStatement {@link PreparedStatement}
		 */
		public PreparedStatementEvent(final ExecutionContext executionContext,
				final PreparedStatement preparedStatement) {
			super(executionContext);
			this.preparedStatement = preparedStatement;
		}

		/**
		 * {@link PreparedStatement}を取得します.
		 *
		 * @return {@link PreparedStatement}
		 */
		public PreparedStatement getPreparedStatement() {
			return this.preparedStatement;
		}
	}

	/**
	 * CallableStatement作成イベント.
	 */
	public static class CallableStatementEvent extends SqlEvent {

		private final CallableStatement callableStatement;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param callableStatement {@link CallableStatement}
		 */
		public CallableStatementEvent(final ExecutionContext executionContext,
				final CallableStatement callableStatement) {
			super(executionContext);
			this.callableStatement = callableStatement;
		}

		/**
		 * {@link CallableStatement}を取得します.
		 *
		 * @return {@link CallableStatement}
		 */
		public CallableStatement getCallableStatement() {
			return this.callableStatement;
		}
	}

	/**
	 * パラメータ編集イベント.
	 */
	public static class ParameterEvent extends SqlEvent {

		private final Parameter parameter;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param parameter {@link Parameter}
		 */
		public ParameterEvent(final ExecutionContext executionContext, final Parameter parameter) {
			super(executionContext);
			this.parameter = parameter;
		}

		/**
		 * {@link Parameter}を取得します.
		 *
		 * @return {@link Parameter}
		 */
		public Parameter getParameter() {
			return this.parameter;
		}
	}

	/**
	 * 出力パラメータ編集イベント.
	 */
	public static class OutParameterEvent extends SqlEvent {

		private final String key;

		private final Object value;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param key パラメータのキー
		 * @param value パラメータの値
		 */
		public OutParameterEvent(final ExecutionContext executionContext, final String key, final Object value) {
			super(executionContext);
			this.key = key;
			this.value = value;
		}

		/**
		 * パラメータのキーを取得します.
		 *
		 * @return キー
		 */
		public String getKey() {
			return this.key;
		}

		/**
		 * パラメータの値を取得します.
		 *
		 * @return 値
		 */
		public Object getValue() {
			return this.value;
		}
	}

	/**
	 * INSERTパラメータ編集イベント.
	 */
	public static class InsertParameterEvent extends SqlEvent {

		private final Object entity;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param entity エンティティ
		 */
		public InsertParameterEvent(final ExecutionContext executionContext, final Object entity) {
			super(executionContext);
			this.entity = entity;
		}

		/**
		 * エンティティを取得します.
		 *
		 * @return エンティティ
		 */
		public Object getEntity() {
			return this.entity;
		}
	}

	/**
	 * UPDATEパラメータ編集イベント.
	 */
	public static class UpdateParameterEvent extends SqlEvent {

		private final Object entity;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param entity エンティティ
		 */
		public UpdateParameterEvent(final ExecutionContext executionContext, final Object entity) {
			super(executionContext);
			this.entity = entity;
		}

		/**
		 * エンティティを取得します.
		 *
		 * @return エンティティ
		 */
		public Object getEntity() {
			return this.entity;
		}
	}

	/**
	 * DELETEパラメータ編集イベント.
	 */
	public static class DeleteParameterEvent extends SqlEvent {

		private final Object entity;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param entity エンティティ
		 */
		public DeleteParameterEvent(final ExecutionContext executionContext, final Object entity) {
			super(executionContext);
			this.entity = entity;
		}

		/**
		 * エンティティを取得します.
		 *
		 * @return エンティティ
		 */
		public Object getEntity() {
			return this.entity;
		}
	}

	/**
	 * BULK-INSERTパラメータ編集イベント.
	 */
	public static class BulkInsertParameterEvent extends SqlEvent {

		private final Object entity;

		private final int entityIndex;

		/**
		 * コンストラクタ.
		 *
		 * @param executionContext {@link ExecutionContext}
		 * @param entity エンティティ
		 * @param entityIndex エンティティのインデックス
		 */
		public BulkInsertParameterEvent(final ExecutionContext executionContext, final Object entity,
				final int entityIndex) {
			super(executionContext);
			this.entity = entity;
			this.entityIndex = entityIndex;
		}

		/**
		 * エンティティを取得します.
		 *
		 * @return エンティティ
		 */
		public Object getEntity() {
			return this.entity;
		}

		/**
		 * エンティティのインデックスを取得します.
		 *
		 * @return エンティティのインデックス
		 */
		public int getEntityIndex() {
			return this.entityIndex;
		}
	}
}
