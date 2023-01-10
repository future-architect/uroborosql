/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

import jp.co.future.uroborosql.event.subscriber.EventSubscriber;

/**
 * イベントリスナ格納オブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class EventListenerHolder {
	/** ExecutionContext初期化後イベントリスナ. */
	private final List<Consumer<AfterInitializeExecutionContextEvent>> afterInitializeExecutionContextListeners = new ArrayList<>();
	/** パラメータ設定前イベントリスナ. */
	private final List<Consumer<BeforeSetParameterEvent>> beforeSetParameterListeners = new ArrayList<>();
	/** SQL変換前イベントリスナ. */
	private final List<Consumer<BeforeTransformSqlEvent>> beforeTransformSqlListeners = new ArrayList<>();
	/** DAO Query時パラメータ設定後イベントリスナ. */
	private final List<Consumer<AfterSetDaoQueryParameterEvent>> afterSetDaoQueryParameterListeners = new ArrayList<>();
	/** DAO Update時パラメータ設定後イベントリスナ. */
	private final List<Consumer<AfterSetDaoUpdateParameterEvent>> afterSetDaoUpdateParameterListeners = new ArrayList<>();
	/** 出力パラメータ取得後イベントリスナ. */
	private final List<Consumer<AfterGetOutParameterEvent>> afterGetOutParameterListeners = new ArrayList<>();
	/** Entity Insert時パラメータ設定後イベントリスナ. */
	private final List<Consumer<AfterSetEntityInsertParameterEvent>> afterSetEntityInsertParameterListeners = new ArrayList<>();
	/** Entity Update時パラメータ設定後イベントリスナ. */
	private final List<Consumer<AfterSetEntityUpdateParameterEvent>> afterSetEntityUpdateParameterListeners = new ArrayList<>();
	/** Entity Delete時パラメータ設定後イベントリスナ. */
	private final List<Consumer<AfterSetEntityDeleteParameterEvent>> afterSetEntityDeleteParameterListeners = new ArrayList<>();
	/** Entity BulkInsert時パラメータ設定後イベントリスナ. */
	private final List<Consumer<AfterSetEntityBulkInsertParameterEvent>> afterSetEntityBulkInsertParameterListeners = new ArrayList<>();
	/** PreparedStatement生成後イベントリスナ. */
	private final List<ExecutionConsumer<AfterCreatePreparedStatementEvent>> afterCreatePreparedStatementListeners = new ArrayList<>();
	/** CallableStatement生成後イベントリスナ. */
	private final List<ExecutionConsumer<AfterCreateCallableStatementEvent>> afterCreateCallableStatementListeners = new ArrayList<>();
	/** SQLQuery実行後イベントリスナ. */
	private final List<ExecutionConsumer<SqlQueryEvent>> sqlQueryListeners = new ArrayList<>();
	/** SQLUpdate実行後イベントリスナ. */
	private final List<ExecutionConsumer<SqlUpdateEvent>> sqlUpdateListeners = new ArrayList<>();
	/** SQLBatch実行後イベントリスナ. */
	private final List<ExecutionConsumer<SqlBatchEvent>> sqlBatchListeners = new ArrayList<>();
	/** Procedure実行後イベントリスナ. */
	private final List<ExecutionConsumer<ProcedureEvent>> procedureListeners = new ArrayList<>();
	/** トランザクション開始後イベントリスナ. */
	private final List<Consumer<AfterBeginTransactionEvent>> afterBeginTransactionListeners = new ArrayList<>();
	/** トランザクション終了前イベントリスナ. */
	private final List<Consumer<BeforeEndTransactionEvent>> beforeEndTransactionListeners = new ArrayList<>();
	/** コミット前イベントリスナ. */
	private final List<Consumer<BeforeCommitEvent>> beforeCommitListeners = new ArrayList<>();
	/** コミット後イベントリスナ. */
	private final List<Consumer<AfterCommitEvent>> afterCommitListeners = new ArrayList<>();
	/** ロールバック前イベントリスナ. */
	private final List<Consumer<BeforeRollbackEvent>> beforeRollbackListeners = new ArrayList<>();
	/** ロールバック後イベントリスナ. */
	private final List<Consumer<AfterRollbackEvent>> afterRollbackListeners = new ArrayList<>();

	/** EventSubscriberリスト. */
	private final List<EventSubscriber> eventSubscribers = new ArrayList<>();

	/**
	 * コンストラクタ.
	 */
	public EventListenerHolder() {
	}

	/**
	 * 初期化処理.
	 */
	public void initialize() {
		// do nothing
	}

	/**
	 * EventSubscriberを追加し、EventSubscriberによるイベント登録を行う.
	 * @param eventSubscriber EventSubscriber
	 * @return
	 */
	public EventListenerHolder addEventSubscriber(EventSubscriber eventSubscriber) {
		this.eventSubscribers.add(eventSubscriber);
		eventSubscriber.initialize();
		eventSubscriber.subscribe(this);
		return this;
	}

	/**
	 * EventSubscriberを追加し、EventSubscriberによるイベント削除を行う.
	 * @param eventSubscriber EventSubscriber
	 * @return
	 */
	public EventListenerHolder removeEventSubscriber(EventSubscriber eventSubscriber) {
		eventSubscriber.unsubscribe(this);
		this.eventSubscribers.remove(eventSubscriber);
		return this;
	}

	/**
	 * ExecutionContext初期化後イベントリスナの追加
	 * @param listener ExecutionContext初期化後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterInitializeExecutionContextListener(
			final Consumer<AfterInitializeExecutionContextEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterInitializeExecutionContextListeners.add(listener);
		return this;
	}

	/**
	 * パラメータ設定前イベントリスナの追加
	 * @param listener パラメータ設定前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeSetParameterListener(final Consumer<BeforeSetParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeSetParameterListeners.add(listener);
		return this;
	}

	/**
	 * SQL変換前イベントリスナの追加
	 * @param listener SQL変換前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeTransformSqlListener(final Consumer<BeforeTransformSqlEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeTransformSqlListeners.add(listener);
		return this;
	}

	/**
	 * DAO Query時パラメータ設定後イベントリスナの追加
	 * @param listener DAO Query時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterSetDaoQueryParameterListener(
			final Consumer<AfterSetDaoQueryParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetDaoQueryParameterListeners.add(listener);
		return this;
	}

	/**
	 * DAO Update時パラメータ設定後イベントリスナの追加
	 * @param listener DAO Update時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterSetDaoUpdateParameterListener(
			final Consumer<AfterSetDaoUpdateParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetDaoUpdateParameterListeners.add(listener);
		return this;
	}

	/**
	 * 出力パラメータ取得後イベントリスナの追加
	 * @param listener 出力パラメータ取得後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterGetOutParameterListener(final Consumer<AfterGetOutParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterGetOutParameterListeners.add(listener);
		return this;
	}

	/**
	 * Entity Insert時パラメータ設定後イベントリスナの追加
	 * @param listener Entity Insert時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterSetEntityInsertParameterListener(
			final Consumer<AfterSetEntityInsertParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetEntityInsertParameterListeners.add(listener);
		return this;
	}

	/**
	 * Entity Update時パラメータ設定後イベントリスナの追加
	 * @param listener Entity Update時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterSetEntityUpdateParameterListener(
			final Consumer<AfterSetEntityUpdateParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetEntityUpdateParameterListeners.add(listener);
		return this;
	}

	/**
	 * Entity Delete時パラメータ設定後イベントリスナの追加
	 * @param listener Entity Delete時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterSetEntityDeleteParameterListener(
			final Consumer<AfterSetEntityDeleteParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetEntityDeleteParameterListeners.add(listener);
		return this;
	}

	/**
	 * Entity BulkInsert時パラメータ設定後イベントリスナの追加
	 * @param listener Entity BulkInsert時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterSetEntityBulkInsertParameterListener(
			final Consumer<AfterSetEntityBulkInsertParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetEntityBulkInsertParameterListeners.add(listener);
		return this;
	}

	/**
	 * PreparedStatement生成後イベントリスナの追加
	 * @param listener PreparedStatement生成後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterCreatePreparedStatementListener(
			final ExecutionConsumer<AfterCreatePreparedStatementEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterCreatePreparedStatementListeners.add(listener);
		return this;
	}

	/**
	 * CallableStatement生成後イベントリスナの追加
	 * @param listener CallableStatement生成後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterCreateCallableStatementListener(
			final ExecutionConsumer<AfterCreateCallableStatementEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterCreateCallableStatementListeners.add(listener);
		return this;
	}

	/**
	 * SQLQuery実行後イベントリスナの追加
	 * @param listener SQLQuery実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addSqlQueryListener(final ExecutionConsumer<SqlQueryEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.sqlQueryListeners.add(listener);
		return this;
	}

	/**
	 * SQLUpdate実行後イベントリスナの追加
	 * @param listener SQLUpdate実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addSqlUpdateListener(final ExecutionConsumer<SqlUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.sqlUpdateListeners.add(listener);
		return this;
	}

	/**
	 * SQLBatch実行後イベントリスナの追加
	 * @param listener SQLBatch実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addSqlBatchListener(final ExecutionConsumer<SqlBatchEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.sqlBatchListeners.add(listener);
		return this;
	}

	/**
	 * Procedure実行後イベントリスナの追加
	 * @param listener Procedure実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addProcedureListener(final ExecutionConsumer<ProcedureEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.procedureListeners.add(listener);
		return this;
	}

	/**
	 * トランザクション開始後イベントリスナの追加
	 * @param listener トランザクション開始後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterBeginTransactionListener(final Consumer<AfterBeginTransactionEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterBeginTransactionListeners.add(listener);
		return this;
	}

	/**
	 * トランザクション終了前イベントリスナの追加
	 * @param listener トランザクション終了前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeEndTransactionListener(final Consumer<BeforeEndTransactionEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEndTransactionListeners.add(listener);
		return this;
	}

	/**
	 * コミット前イベントリスナの追加
	 * @param listener コミット前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeCommitListener(final Consumer<BeforeCommitEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeCommitListeners.add(listener);
		return this;
	}

	/**
	 * コミット後イベントリスナの追加
	 * @param listener コミット後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterCommitListener(final Consumer<AfterCommitEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterCommitListeners.add(listener);
		return this;
	}

	/**
	 * ロールバック前イベントリスナの追加
	 * @param listener ロールバック前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeRollbackListener(final Consumer<BeforeRollbackEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeRollbackListeners.add(listener);
		return this;
	}

	/**
	 * ロールバック後イベントリスナの追加
	 * @param listener ロールバック後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterRollbackListener(final Consumer<AfterRollbackEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterRollbackListeners.add(listener);
		return this;
	}

	/**
	 * ExecutionContext初期化後イベントリスナの削除
	 * @param listener ExecutionContext初期化後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterInitializeExecutionContextListener(
			final Consumer<AfterInitializeExecutionContextEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterInitializeExecutionContextListeners.remove(listener);
		return this;
	}

	/**
	 * パラメータ設定前イベントリスナの削除
	 * @param listener パラメータ設定前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeSetParameterListener(final Consumer<BeforeSetParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeSetParameterListeners.remove(listener);
		return this;
	}

	/**
	 * SQL変換前イベントリスナの削除
	 * @param listener SQL変換前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeTransformSqlListener(final Consumer<BeforeTransformSqlEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeTransformSqlListeners.remove(listener);
		return this;
	}

	/**
	 * DAO Query時パラメータ設定後イベントリスナの削除
	 * @param listener DAO Query時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterSetDaoQueryParameterListener(
			final Consumer<AfterSetDaoQueryParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetDaoQueryParameterListeners.remove(listener);
		return this;
	}

	/**
	 * DAO Update時パラメータ設定後イベントリスナの削除
	 * @param listener DAO Update時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterSetDaoUpdateParameterListener(
			final Consumer<AfterSetDaoUpdateParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetDaoUpdateParameterListeners.remove(listener);
		return this;
	}

	/**
	 * 出力パラメータ取得後イベントリスナの削除
	 * @param listener 出力パラメータ取得後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterGetOutParameterListener(final Consumer<AfterGetOutParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterGetOutParameterListeners.remove(listener);
		return this;
	}

	/**
	 * Entity Insert時パラメータ設定後イベントリスナの削除
	 * @param listener Entity Insert時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterSetEntityInsertParameterListener(
			final Consumer<AfterSetEntityInsertParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetEntityInsertParameterListeners.remove(listener);
		return this;
	}

	/**
	 * Entity Update時パラメータ設定後イベントリスナの削除
	 * @param listener Entity Update時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterSetEntityUpdateParameterListener(
			final Consumer<AfterSetEntityUpdateParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetEntityUpdateParameterListeners.remove(listener);
		return this;
	}

	/**
	 * Entity Delete時パラメータ設定後イベントリスナの削除
	 * @param listener Entity Delete時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterSetEntityDeleteParameterListener(
			final Consumer<AfterSetEntityDeleteParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetEntityDeleteParameterListeners.remove(listener);
		return this;
	}

	/**
	 * Entity BulkInsert時パラメータ設定後イベントリスナの削除
	 * @param listener Entity BulkInsert時パラメータ設定後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterSetEntityBulkInsertParameterListener(
			final Consumer<AfterSetEntityBulkInsertParameterEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSetEntityBulkInsertParameterListeners.remove(listener);
		return this;
	}

	/**
	 * PreparedStatement生成後イベントリスナの削除
	 * @param listener PreparedStatement生成後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterCreatePreparedStatementListener(
			final ExecutionConsumer<AfterCreatePreparedStatementEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterCreatePreparedStatementListeners.remove(listener);
		return this;
	}

	/**
	 * CallableStatement生成後イベントリスナの削除
	 * @param listener CallableStatement生成後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterCreateCallableStatementListener(
			final ExecutionConsumer<AfterCreateCallableStatementEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterCreateCallableStatementListeners.remove(listener);
		return this;
	}

	/**
	 * SQLQuery実行後イベントリスナの削除
	 * @param listener SQLQuery実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeSqlQueryListener(final ExecutionConsumer<SqlQueryEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.sqlQueryListeners.remove(listener);
		return this;
	}

	/**
	 * SQLUpdate実行後イベントリスナの削除
	 * @param listener SQLUpdate実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeSqlUpdateListener(final ExecutionConsumer<SqlUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.sqlUpdateListeners.remove(listener);
		return this;
	}

	/**
	 * SQLBatch実行後イベントリスナの削除
	 * @param listener SQLBatch実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeSqlBatchListener(final ExecutionConsumer<SqlBatchEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.sqlBatchListeners.remove(listener);
		return this;
	}

	/**
	 * Procedure実行後イベントリスナの削除
	 * @param listener Procedure実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeProcedureListener(final ExecutionConsumer<ProcedureEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.procedureListeners.remove(listener);
		return this;
	}

	/**
	 * トランザクション開始後イベントリスナの削除
	 * @param listener トランザクション開始後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterBeginTransactionListener(
			final Consumer<AfterBeginTransactionEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterBeginTransactionListeners.remove(listener);
		return this;
	}

	/**
	 * トランザクション終了前イベントリスナの削除
	 * @param listener トランザクション終了前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeEndTransactionListener(final Consumer<BeforeEndTransactionEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEndTransactionListeners.remove(listener);
		return this;
	}

	/**
	 * コミット前イベントリスナの削除
	 * @param listener コミット前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeCommitListener(final Consumer<BeforeCommitEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeCommitListeners.remove(listener);
		return this;
	}

	/**
	 * コミット後イベントリスナの削除
	 * @param listener コミット後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterCommitListener(final Consumer<AfterCommitEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterCommitListeners.remove(listener);
		return this;
	}

	/**
	 * ロールバック前イベントリスナの削除
	 * @param listener ロールバック前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeRollbackListener(final Consumer<BeforeRollbackEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeRollbackListeners.remove(listener);
		return this;
	}

	/**
	 * ロールバック後イベントリスナの削除
	 * @param listener ロールバック後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterRollbackListener(final Consumer<AfterRollbackEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterRollbackListeners.remove(listener);
		return this;
	}

	/**
	 * ExecutionContext初期化後イベントリスナリストの取得.
	 * @return ExecutionContext初期化後イベントリスナリスト
	 */
	public List<Consumer<AfterInitializeExecutionContextEvent>> getAfterInitializeExecutionContextListeners() {
		return afterInitializeExecutionContextListeners;
	}

	/**
	 * パラメータ設定前イベントリスナリストの取得.
	 * @return パラメータ設定前イベントリスナリスト
	 */
	public List<Consumer<BeforeSetParameterEvent>> getBeforeSetParameterListeners() {
		return beforeSetParameterListeners;
	}

	/**
	 * SQL変換前イベントリスナリストの取得.
	 * @return SQL変換前イベントリスナリスト
	 */
	public List<Consumer<BeforeTransformSqlEvent>> getBeforeTransformSqlListeners() {
		return beforeTransformSqlListeners;
	}

	/**
	 * DAO Query時パラメータ設定後イベントリスナリストの取得.
	 * @return DAO Query時パラメータ設定後イベントリスナリスト
	 */
	public List<Consumer<AfterSetDaoQueryParameterEvent>> getAfterSetDaoQueryParameterListeners() {
		return afterSetDaoQueryParameterListeners;
	}

	/**
	 * DAO Update時パラメータ設定後イベントリスナリストの取得.
	 * @return DAO Update時パラメータ設定後イベントリスナリスト
	 */
	public List<Consumer<AfterSetDaoUpdateParameterEvent>> getAfterSetDaoUpdateParameterListeners() {
		return afterSetDaoUpdateParameterListeners;
	}

	/**
	 * 出力パラメータ取得後イベントリスナリストの取得.
	 * @return 出力パラメータ取得後イベントリスナリスト
	 */
	public List<Consumer<AfterGetOutParameterEvent>> getAfterGetOutParameterListeners() {
		return afterGetOutParameterListeners;
	}

	/**
	 * Entity Insert時パラメータ設定後イベントリスナリストの取得.
	 * @return Entity Insert時パラメータ設定後イベントリスナリスト
	 */
	public List<Consumer<AfterSetEntityInsertParameterEvent>> getAfterSetEntityInsertParameterListeners() {
		return afterSetEntityInsertParameterListeners;
	}

	/**
	 * Entity Update時パラメータ設定後イベントリスナリストの取得.
	 * @return Entity Update時パラメータ設定後イベントリスナリスト
	 */
	public List<Consumer<AfterSetEntityUpdateParameterEvent>> getAfterSetEntityUpdateParameterListeners() {
		return afterSetEntityUpdateParameterListeners;
	}

	/**
	 * Entity Delete時パラメータ設定後イベントリスナリストの取得.
	 * @return Entity Delete時パラメータ設定後イベントリスナリスト
	 */
	public List<Consumer<AfterSetEntityDeleteParameterEvent>> getAfterSetEntityDeleteParameterListeners() {
		return afterSetEntityDeleteParameterListeners;
	}

	/**
	 * Entity BulkInsert時パラメータ設定後イベントリスナリストの取得.
	 * @return Entity BulkInsert時パラメータ設定後イベントリスナリスト
	 */
	public List<Consumer<AfterSetEntityBulkInsertParameterEvent>> getAfterSetEntityBulkInsertParameterListeners() {
		return afterSetEntityBulkInsertParameterListeners;
	}

	/**
	 * PreparedStatement生成後イベントリスナリストの取得.
	 * @return PreparedStatement生成後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterCreatePreparedStatementEvent>> getAfterCreatePreparedStatementListeners() {
		return afterCreatePreparedStatementListeners;
	}

	/**
	 * CallableStatement生成後イベントリスナリストの取得.
	 * @return CallableStatement生成後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterCreateCallableStatementEvent>> getAfterCreateCallableStatementListeners() {
		return afterCreateCallableStatementListeners;
	}

	/**
	 * SQLQuery実行後イベントリスナリストの取得.
	 * @return SQLQuery実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<SqlQueryEvent>> getSqlQueryListeners() {
		return sqlQueryListeners;
	}

	/**
	 * SQLUpdate実行後イベントリスナリストの取得.
	 * @return SQLUpdate実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<SqlUpdateEvent>> getSqlUpdateListeners() {
		return sqlUpdateListeners;
	}

	/**
	 * SQLBatch実行後イベントリスナリストの取得.
	 * @return SQLBatch実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<SqlBatchEvent>> getSqlBatchListeners() {
		return sqlBatchListeners;
	}

	/**
	 * Procedure実行後イベントリスナリストの取得.
	 * @return Procedure実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<ProcedureEvent>> getProcedureListeners() {
		return procedureListeners;
	}

	/**
	 * トランザクション開始後イベントリスナリストの取得.
	 * @return トランザクション開始後イベントリスナリスト
	 */
	public List<Consumer<AfterBeginTransactionEvent>> getAfterBeginTransactionListeners() {
		return afterBeginTransactionListeners;
	}

	/**
	 * トランザクション終了前イベントリスナリストの取得.
	 * @return トランザクション終了前イベントリスナリスト
	 */
	public List<Consumer<BeforeEndTransactionEvent>> getBeforeEndTransactionListeners() {
		return beforeEndTransactionListeners;
	}

	/**
	 * コミット前イベントリスナリストの取得.
	 * @return コミット前イベントリスナリスト
	 */
	public List<Consumer<BeforeCommitEvent>> getBeforeCommitListeners() {
		return beforeCommitListeners;
	}

	/**
	 * コミット後イベントリスナリストの取得.
	 * @return コミット後イベントリスナリスト
	 */
	public List<Consumer<AfterCommitEvent>> getAfterCommitListeners() {
		return afterCommitListeners;
	}

	/**
	 * ロールバック前イベントリスナリストの取得.
	 * @return ロールバック前イベントリスナリスト
	 */
	public List<Consumer<BeforeRollbackEvent>> getBeforeRollbackListeners() {
		return beforeRollbackListeners;
	}

	/**
	 * ロールバック後イベントリスナリストの取得.
	 * @return ロールバック後イベントリスナリスト
	 */
	public List<Consumer<AfterRollbackEvent>> getAfterRollbackListeners() {
		return afterRollbackListeners;
	}

	/**
	 * ExecutionContext初期化後イベントリスナがあるかどうか.
	 * @return ExecutionContext初期化後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterInitializeExecutionContextListener() {
		return !afterInitializeExecutionContextListeners.isEmpty();
	}

	/**
	 *  パラメータ設定前イベントリスナがあるかどうか.
	 * @return パラメータ設定前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeSetParameterListener() {
		return !beforeSetParameterListeners.isEmpty();
	}

	/**
	 * SQL変換前イベントリスナがあるかどうか.
	 * @return SQL変換前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeTransformSqlListener() {
		return !beforeTransformSqlListeners.isEmpty();
	}

	/**
	 * DAO Query時パラメータ設定後イベントリスナがあるかどうか.
	 * @return DAO Query時パラメータ設定後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterSetDaoQueryParameterListener() {
		return !afterSetDaoQueryParameterListeners.isEmpty();
	}

	/**
	 * DAO Update時パラメータ設定後イベントリスナがあるかどうか.
	 * @return DAO Update時パラメータ設定後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterSetDaoUpdateParameterListener() {
		return !afterSetDaoUpdateParameterListeners.isEmpty();
	}

	/**
	 * 出力パラメータ取得後イベントリスナがあるかどうか.
	 * @return 出力パラメータ取得後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterGetOutParameterListener() {
		return !afterGetOutParameterListeners.isEmpty();
	}

	/**
	 * Entity Insert時パラメータ設定後イベントリスナがあるかどうか.
	 * @return Entity Insert時パラメータ設定後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterSetEntityInsertParameterListener() {
		return !afterSetEntityInsertParameterListeners.isEmpty();
	}

	/**
	 * Entity Update時パラメータ設定後イベントリスナがあるかどうか.
	 * @return Entity Update時パラメータ設定後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterSetEntityUpdateParameterListener() {
		return !afterSetEntityUpdateParameterListeners.isEmpty();
	}

	/**
	 * Entity Delete時パラメータ設定後イベントリスナがあるかどうか.
	 * @return Entity Delete時パラメータ設定後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterSetEntityDeleteParameterListener() {
		return !afterSetEntityDeleteParameterListeners.isEmpty();
	}

	/**
	 * Entity BulkInsert時パラメータ設定後イベントリスナがあるかどうか.
	 * @return Entity BulkInsert時パラメータ設定後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterSetEntityBulkInsertParameterListener() {
		return !afterSetEntityBulkInsertParameterListeners.isEmpty();
	}

	/**
	 * PreparedStatement生成後イベントリスナがあるかどうか.
	 * @return PreparedStatement生成後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterCreatePreparedStatementListener() {
		return !afterCreatePreparedStatementListeners.isEmpty();
	}

	/**
	 * CallableStatement生成後イベントリスナがあるかどうか.
	 * @return CallableStatement生成後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterCreateCallableStatementListener() {
		return !afterCreateCallableStatementListeners.isEmpty();
	}

	/**
	 * SQLQuery実行後イベントリスナがあるかどうか.
	 * @return SQLQuery実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasSqlQueryListener() {
		return !sqlQueryListeners.isEmpty();
	}

	/**
	 * SQLUpdate実行後イベントリスナがあるかどうか.
	 * @return SQLUpdate実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasSqlUpdateListener() {
		return !sqlUpdateListeners.isEmpty();
	}

	/**
	 * SQLBatch実行後イベントリスナがあるかどうか.
	 * @return SQLBatch実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasSqlBatchListener() {
		return !sqlBatchListeners.isEmpty();
	}

	/**
	 * Procedure実行後イベントリスナがあるかどうか.
	 * @return Procedure実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasProcedureListener() {
		return !procedureListeners.isEmpty();
	}

	/**
	 * トランザクション開始後イベントリスナがあるかどうか.
	 * @return トランザクション開始後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterBeginTransactionListener() {
		return !afterBeginTransactionListeners.isEmpty();
	}

	/**
	 * トランザクション終了前イベントリスナがあるかどうか.
	 * @return トランザクション終了前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeEndTransactionListener() {
		return !beforeEndTransactionListeners.isEmpty();
	}

	/**
	 * コミット前イベントリスナがあるかどうか.
	 * @return コミット前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeCommitListener() {
		return !beforeCommitListeners.isEmpty();
	}

	/**
	 * コミット後イベントリスナがあるかどうか.
	 * @return コミット後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterCommitListener() {
		return !afterCommitListeners.isEmpty();
	}

	/**
	 * ロールバック前イベントリスナがあるかどうか.
	 * @return ロールバック前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeRollbackListener() {
		return !beforeRollbackListeners.isEmpty();
	}

	/**
	 * ロールバック後イベントリスナがあるかどうか.
	 * @return ロールバック後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterRollbackListener() {
		return !afterRollbackListeners.isEmpty();
	}

}
