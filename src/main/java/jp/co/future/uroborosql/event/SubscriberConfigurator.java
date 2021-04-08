/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.lang.reflect.ParameterizedType;
import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

/**
 * イベントサブスクライバ設定クラス.
 *
 * @author yanagihara
 */
public class SubscriberConfigurator {

	private static final Logger LOG = LoggerFactory.getLogger(SubscriberConfigurator.class);

	protected final List<Function<TransformSqlEvent, String>> transformSqlSubs = new ArrayList<>();

	protected final List<Function<ParameterEvent, Parameter>> parameterSubs = new ArrayList<>();

	protected final List<Function<OutParameterEvent, Object>> outParameterSubs = new ArrayList<>();

	protected final List<SqlEventFunction<PreparedStatementEvent, PreparedStatement>> preparedStatementSubs = new ArrayList<>();

	protected final List<SqlEventFunction<CallableStatementEvent, CallableStatement>> callableStatementSubs = new ArrayList<>();

	protected final List<Consumer<InsertParameterEvent>> setInsertParamsSubs = new ArrayList<>();

	protected final List<Consumer<UpdateParameterEvent>> setUpdateParamsSubs = new ArrayList<>();

	protected final List<Consumer<DeleteParameterEvent>> setDeleteParamsSubs = new ArrayList<>();

	protected final List<Consumer<BulkInsertParameterEvent>> setBulkInsertParamsSubs = new ArrayList<>();

	protected final List<Consumer<ExecutionContext>> queryAutoParameterBinders = new ArrayList<>();

	protected final List<Consumer<ExecutionContext>> updateAutoParameterBinders = new ArrayList<>();

	protected final List<Consumer<BeforeTransactionEvent>> beforeTransactionSubs = new ArrayList<>();

	protected final List<Consumer<AfterTransactionEvent>> afterTransactionSubs = new ArrayList<>();

	protected final List<Consumer<BeforeCommitEvent>> beforeCommitSubs = new ArrayList<>();

	protected final List<Consumer<AfterCommitEvent>> afterCommitSubs = new ArrayList<>();

	protected final List<Consumer<BeforeRollbackEvent>> beforeRollbackSubs = new ArrayList<>();

	protected final List<Consumer<AfterRollbackEvent>> afterRollbackSubs = new ArrayList<>();

	protected final List<ResultEventFunction<QueryResultEvent, ResultSet>> queryResultSubs = new ArrayList<>();

	protected final List<ResultEventFunction<UpdateResultEvent, Integer>> updateResultSubs = new ArrayList<>();

	protected final List<ResultEventFunction<BatchResultEvent, int[]>> batchResultSubs = new ArrayList<>();

	protected final List<ResultEventFunction<ProcedureResultEvent, Boolean>> procedureResultSubs = new ArrayList<>();

	@FunctionalInterface
	public interface SqlEventFunction<T extends SqlEvent, R extends PreparedStatement> {
		R apply(T t) throws SQLException;
	}

	@FunctionalInterface
	public interface ResultEventFunction<T extends ResultEvent, R> {
		R apply(T t) throws SQLException;
	}

	/**
	 * パラメータ編集イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnParameter(final Function<ParameterEvent, Parameter> subscriber) {
		Objects.requireNonNull(subscriber, "doOnParameter");
		parameterSubs.add(subscriber);
		return this;
	}

	/**
	 * 出力パラメータ編集イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnOutParameter(final Function<OutParameterEvent, Object> subscriber) {
		Objects.requireNonNull(subscriber, "doOnOutParameter");
		outParameterSubs.add(subscriber);
		return this;
	}

	/**
	 * PreparedStatement作成イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnPreparedStatement(
			final SqlEventFunction<PreparedStatementEvent, PreparedStatement> subscriber) {
		Objects.requireNonNull(subscriber, "doOnPreparedStatement");
		preparedStatementSubs.add(subscriber);
		return this;
	}

	/**
	 * CallableStatement作成イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnCallableStatement(
			final SqlEventFunction<CallableStatementEvent, CallableStatement> subscriber) {
		Objects.requireNonNull(subscriber, "doOnCallableStatement");
		callableStatementSubs.add(subscriber);
		return this;
	}

	/**
	 * SQL変換イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnTransformSql(final Function<TransformSqlEvent, String> subscriber) {
		Objects.requireNonNull(subscriber, "doOnTransformSql");
		transformSqlSubs.add(subscriber);
		return this;
	}

	/**
	 * 自動パラメータバインド関数(query用)の追加.
	 *
	 * @param binder 自動パラメータバインド関数
	 * @return Subscribers
	 */
	public SubscriberConfigurator addQueryAutoParameterBinder(final Consumer<ExecutionContext> binder) {
		Objects.requireNonNull(binder, "addQueryAutoParameterBinder");
		queryAutoParameterBinders.add(binder);
		return this;
	}

	/**
	 * 自動パラメータバインド関数(update/batch/proc用)の追加.
	 *
	 * @param binder 自動パラメータバインド関数
	 * @return Subscribers
	 */
	public SubscriberConfigurator addUpdateAutoParameterBinder(final Consumer<ExecutionContext> binder) {
		Objects.requireNonNull(binder, "addUpdateAutoParameterBinder");
		updateAutoParameterBinders.add(binder);
		return this;
	}

	/**
	 * トランザクション開始前イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doBeforeTransaction(final Consumer<BeforeTransactionEvent> subscriber) {
		Objects.requireNonNull(subscriber, "doBeforeTransaction");
		beforeTransactionSubs.add(subscriber);
		return this;
	}

	/**
	 * トランザクション終了後イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doAfterTransaction(final Consumer<AfterTransactionEvent> subscriber) {
		Objects.requireNonNull(subscriber, "doAfterTransaction");
		afterTransactionSubs.add(subscriber);
		return this;
	}

	/**
	 * コミット実行前イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doBeforeCommit(final Consumer<BeforeCommitEvent> subscriber) {
		Objects.requireNonNull(subscriber, "doBeforeCommit");
		beforeCommitSubs.add(subscriber);
		return this;
	}

	/**
	 * コミット実行後イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doAfterCommit(final Consumer<AfterCommitEvent> subscriber) {
		Objects.requireNonNull(subscriber, "doAfterCommit");
		afterCommitSubs.add(subscriber);
		return this;
	}

	/**
	 * ロールバック実行前イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doBeforeRollback(final Consumer<BeforeRollbackEvent> subscriber) {
		Objects.requireNonNull(subscriber, "doBeforeRollback");
		beforeRollbackSubs.add(subscriber);
		return this;
	}

	/**
	 * ロールバック実行後イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doAfterRollback(final Consumer<AfterRollbackEvent> subscriber) {
		Objects.requireNonNull(subscriber, "doAfterRollback");
		afterRollbackSubs.add(subscriber);
		return this;
	}

	/**
	 * エンティティ編集イベント[INSERT]の登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnInsertParams(final Consumer<InsertParameterEvent> subscriber) {
		Objects.requireNonNull(subscriber, "doOnInsertParams");
		setInsertParamsSubs.add(subscriber);
		return this;
	}

	/**
	 * エンティティ編集イベント[UPDATE]の登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnUpdateParams(final Consumer<UpdateParameterEvent> subscriber) {
		Objects.requireNonNull(subscriber, "doOnUpdateParams");
		setUpdateParamsSubs.add(subscriber);
		return this;
	}

	/**
	 * エンティティ編集イベント[DELETE]の登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnDeleteParams(final Consumer<DeleteParameterEvent> subscriber) {
		Objects.requireNonNull(subscriber, "doOnDeleteParams");
		setDeleteParamsSubs.add(subscriber);
		return this;
	}

	/**
	 * エンティティ編集イベント[BULK-INSERT]の登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnBulkInsertParams(final Consumer<BulkInsertParameterEvent> subscriber) {
		Objects.requireNonNull(subscriber, "doOnBulkInsertParams");
		setBulkInsertParamsSubs.add(subscriber);
		return this;
	}

	/**
	 * クエリ実行結果イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnQuery(final ResultEventFunction<QueryResultEvent, ResultSet> subscriber) {
		Objects.requireNonNull(subscriber, "doOnQueryResult");
		queryResultSubs.add(subscriber);
		return this;
	}

	/**
	 * 更新実行結果イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnUpdate(final ResultEventFunction<UpdateResultEvent, Integer> subscriber) {
		Objects.requireNonNull(subscriber, "doOnUpdateResult");
		updateResultSubs.add(subscriber);
		return this;
	}

	/**
	 * バッチ実行結果イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnBatch(final ResultEventFunction<BatchResultEvent, int[]> subscriber) {
		Objects.requireNonNull(subscriber, "doOnBatchResult");
		batchResultSubs.add(subscriber);
		return this;
	}

	/**
	 * プロシージャ実行結果イベントサブスクライバの登録.
	 *
	 * @param subscriber サブスクライバ
	 * @return Subscribers
	 */
	public SubscriberConfigurator doOnProcedure(
			final ResultEventFunction<ProcedureResultEvent, Boolean> subscriber) {
		Objects.requireNonNull(subscriber, "doOnProcedureResult");
		procedureResultSubs.add(subscriber);
		return this;
	}

	/**
	 * 登録中のイベントサブスクライバを消去する.
	 *
	 * @return Subscribers
	 */
	public SubscriberConfigurator clearSubscribers() {
		Class<?> cls = getClass();
		while (cls != Object.class) {
			Stream.of(cls.getDeclaredFields())
					.filter(field -> field.getType() == List.class)
					.filter(field -> {
						var typeArg = ((ParameterizedType) field.getGenericType()).getActualTypeArguments()[0];
						var rawTypeArg = ((ParameterizedType) typeArg).getRawType();
						return rawTypeArg == Function.class
								|| rawTypeArg == Consumer.class
								|| rawTypeArg == SqlEventFunction.class
								|| rawTypeArg == ResultEventFunction.class;
					})
					.forEach(listField -> {
						listField.setAccessible(true);
						try {
							var listValue = listField.get(this);
							listValue.getClass().getDeclaredMethod("clear").invoke(listValue);
						} catch (ReflectiveOperationException e) {
							LOG.error(listField.getName(), e);
						}
					});
			cls = cls.getSuperclass();
		}
		return this;
	}
}
