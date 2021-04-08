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
import java.sql.SQLException;
import java.util.Optional;

import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.event.ResultEvent.BatchResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.ProcedureResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.QueryResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.UpdateResultEvent;
import jp.co.future.uroborosql.event.SqlEvent.BulkInsertParameterEvent;
import jp.co.future.uroborosql.event.SqlEvent.CallableStatementEvent;
import jp.co.future.uroborosql.event.SqlEvent.DeleteParameterEvent;
import jp.co.future.uroborosql.event.SqlEvent.InsertParameterEvent;
import jp.co.future.uroborosql.event.SqlEvent.OutParameterEvent;
import jp.co.future.uroborosql.event.SqlEvent.ParameterEvent;
import jp.co.future.uroborosql.event.SqlEvent.PreparedStatementEvent;
import jp.co.future.uroborosql.event.SqlEvent.TransformSqlEvent;
import jp.co.future.uroborosql.event.SqlEvent.UpdateParameterEvent;
import jp.co.future.uroborosql.event.TransactionalEvent.AfterCommitEvent;
import jp.co.future.uroborosql.event.TransactionalEvent.AfterRollbackEvent;
import jp.co.future.uroborosql.event.TransactionalEvent.AfterTransactionEvent;
import jp.co.future.uroborosql.event.TransactionalEvent.BeforeCommitEvent;
import jp.co.future.uroborosql.event.TransactionalEvent.BeforeRollbackEvent;
import jp.co.future.uroborosql.event.TransactionalEvent.BeforeTransactionEvent;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.tx.LocalTransactionContext;

/**
 * サブスクライバに対するイベント発行API.
 *
 * @author yanagihara
 */
public class Subscribers extends SubscriberConfigurator {

	/**
	 * SQL変換イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param originalSql 変換前SQL
	 * @return 変換後SQL
	 */
	public String transformSql(final ExecutionContext executionContext, final String originalSql) {
		var transformedSql = originalSql;
		for (var subscriber : transformSqlSubs) {
			transformedSql = subscriber.apply(new TransformSqlEvent(executionContext, transformedSql));
		}
		return transformedSql;
	}

	/**
	 * パラメータ編集イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param parameter {@link Parameter}
	 * @return Parameter
	 */
	public Parameter parameter(final ExecutionContext executionContext, final Parameter parameter) {
		var editedParameter = parameter;
		for (var subscriber : parameterSubs) {
			editedParameter = subscriber.apply(new ParameterEvent(executionContext, editedParameter));
		}
		return editedParameter;
	}

	/**
	 * 出力パラメータ編集イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param key パラメータのキー
	 * @param value パラメータの値
	 * @return パラメータの値
	 */
	public Object outParameter(final ExecutionContext executionContext, final String key, final Object value) {
		var editedValue = value;
		for (var subscriber : outParameterSubs) {
			editedValue = subscriber.apply(new OutParameterEvent(executionContext, key, editedValue));
		}
		return editedValue;
	}

	/**
	 * PreparedStatement作成イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param preparedStatement {@link PreparedStatement}
	 * @return PreparedStatement
	 * @throws SQLException SQL例外
	 */
	public PreparedStatement preparedStatement(final ExecutionContext executionContext,
			final PreparedStatement preparedStatement) throws SQLException {
		var filteredStatement = preparedStatement;
		for (var subscriber : preparedStatementSubs) {
			filteredStatement = subscriber.apply(new PreparedStatementEvent(executionContext, filteredStatement));
		}
		return filteredStatement;
	}

	/**
	 * CallableStatement作成イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param callableStatement {@link CallableStatement}
	 * @return CallableStatement
	 * @throws SQLException SQL例外
	 */
	public CallableStatement callableStatement(final ExecutionContext executionContext,
			final CallableStatement callableStatement) throws SQLException {
		var filteredStatement = callableStatement;
		for (var subscriber : callableStatementSubs) {
			filteredStatement = subscriber.apply(new CallableStatementEvent(executionContext, filteredStatement));
		}
		return filteredStatement;
	}

	/**
	 * 自動パラメータバインド(query)イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 */
	public void queryAutoParameterBinder(final ExecutionContext executionContext) {
		queryAutoParameterBinders.forEach(s -> s.accept(executionContext));
	}

	/**
	 * 自動パラメータバインド(update/batch/proc)イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 */
	public void updateAutoParameterBinder(final ExecutionContext executionContext) {
		updateAutoParameterBinders.forEach(s -> s.accept(executionContext));
	}

	/**
	 * トランザクション開始前イベントの発行.
	 *
	 * @param txContext {@link LocalTransactionContext}
	 * @param connectionContext {@link ConnectionContext}
	 */
	public void beforeTransaction(final LocalTransactionContext txContext,
			final Optional<ConnectionContext> connectionContext) {
		beforeTransactionSubs.forEach(s -> s.accept(new BeforeTransactionEvent(txContext, connectionContext)));
	}

	/**
	 * トランザクション終了後イベントの発行.
	 *
	 * @param txContext {@link LocalTransactionContext}
	 * @param connectionContext {@link ConnectionContext}
	 */
	public void afterTransaction(final LocalTransactionContext txContext,
			final Optional<ConnectionContext> connectionContext) {
		afterTransactionSubs.forEach(s -> s.accept(new AfterTransactionEvent(txContext, connectionContext)));
	}

	/**
	 * コミット実行前イベントの発行.
	 *
	 * @param txContext {@link LocalTransactionContext}
	 * @param connectionContext {@link ConnectionContext}
	 */
	public void beforeCommit(final LocalTransactionContext txContext,
			final Optional<ConnectionContext> connectionContext) {
		beforeCommitSubs.forEach(s -> s.accept(new BeforeCommitEvent(txContext, connectionContext)));
	}

	/**
	 * コミット実行後イベントの発行.
	 *
	 * @param txContext {@link LocalTransactionContext}
	 * @param connectionContext {@link ConnectionContext}
	 */
	public void afterCommit(final LocalTransactionContext txContext,
			final Optional<ConnectionContext> connectionContext) {
		afterCommitSubs.forEach(s -> s.accept(new AfterCommitEvent(txContext, connectionContext)));
	}

	/**
	 * ロールバック実行前イベントの発行.
	 *
	 * @param txContext {@link LocalTransactionContext}
	 * @param connectionContext {@link ConnectionContext}
	 */
	public void beforeRollback(final LocalTransactionContext txContext,
			final Optional<ConnectionContext> connectionContext) {
		beforeRollbackSubs.forEach(s -> s.accept(new BeforeRollbackEvent(txContext, connectionContext)));
	}

	/**
	 * ロールバック実行後イベントの発行.
	 *
	 * @param txContext {@link LocalTransactionContext}
	 * @param connectionContext {@link ConnectionContext}
	 */
	public void afterRollback(final LocalTransactionContext txContext,
			final Optional<ConnectionContext> connectionContext) {
		afterRollbackSubs.forEach(s -> s.accept(new AfterRollbackEvent(txContext, connectionContext)));
	}

	/**
	 * INSERTパラメータ編集イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param entity エンティティ
	 */
	public void insertParams(final ExecutionContext executionContext, final Object entity) {
		setInsertParamsSubs.forEach(s -> s.accept(new InsertParameterEvent(executionContext, entity)));
	}

	/**
	 * UPDATEパラメータ編集イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param entity エンティティ
	 */
	public void updateParams(final ExecutionContext executionContext, final Object entity) {
		setUpdateParamsSubs.forEach(s -> s.accept(new UpdateParameterEvent(executionContext, entity)));
	}

	/**
	 * DELETEパラメータ編集イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param entity エンティティ
	 */
	public void deleteParams(final ExecutionContext executionContext, final Object entity) {
		setDeleteParamsSubs.forEach(s -> s.accept(new DeleteParameterEvent(executionContext, entity)));
	}

	/**
	 * BULK-INSERTパラメータ編集イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param entity エンティティ
	 * @param entityIndex エンティティのインデックス
	 */
	public void bulkInsertParams(final ExecutionContext executionContext, final Object entity, final int entityIndex) {
		setBulkInsertParamsSubs
				.forEach(s -> s.accept(new BulkInsertParameterEvent(executionContext, entity, entityIndex)));
	}

	/**
	 * クエリ実行結果イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param preparedStatement {@link PreparedStatement}
	 * @param resultSet {@link ResultSet}
	 * @return ResultSet
	 * @throws SQLException SQL例外
	 */
	public ResultSet queryResult(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
			final ResultSet resultSet) throws SQLException {
		var filteredResultSet = resultSet;
		for (var subscriber : queryResultSubs) {
			filteredResultSet = subscriber
					.apply(new QueryResultEvent(executionContext, preparedStatement, filteredResultSet));
		}
		return filteredResultSet;
	}

	/**
	 * 更新実行結果イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param preparedStatement {@link PreparedStatement}
	 * @param result 更新件数
	 * @return 更新件数
	 * @throws SQLException SQL例外
	 */
	public int updateResult(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
			final int result) throws SQLException {
		var filteredResult = result;
		for (var subscriber : updateResultSubs) {
			filteredResult = subscriber
					.apply(new UpdateResultEvent(executionContext, preparedStatement, filteredResult));
		}
		return filteredResult;
	}

	/**
	 * バッチ実行結果イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param preparedStatement {@link PreparedStatement}
	 * @param results 更新件数
	 * @return 更新件数
	 * @throws SQLException SQL例外
	 */
	public int[] batchResult(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
			final int[] results) throws SQLException {
		var filteredResults = results;
		for (var subscriber : batchResultSubs) {
			filteredResults = subscriber
					.apply(new BatchResultEvent(executionContext, preparedStatement, filteredResults));
		}
		return filteredResults;
	}

	/**
	 * プロシージャ実行結果イベントの発行.
	 *
	 * @param executionContext {@link ExecutionContext}
	 * @param callableStatement {@link CallableStatement}
	 * @param result 実行結果
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	public boolean procedureResult(final ExecutionContext executionContext, final CallableStatement callableStatement,
			final boolean result) throws SQLException {
		var filteredResult = result;
		for (var subscriber : procedureResultSubs) {
			filteredResult = subscriber
					.apply(new ProcedureResultEvent(executionContext, callableStatement, filteredResult));
		}
		return filteredResult;
	}
}
