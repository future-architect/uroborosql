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
	/** SQLパース前イベントリスナ. */
	private final List<Consumer<BeforeParseSqlEvent>> beforeParseSqlListeners = new ArrayList<>();
	/** 出力パラメータ取得後イベントリスナ. */
	private final List<Consumer<AfterGetOutParameterEvent>> afterGetOutParameterListeners = new ArrayList<>();
	/** PreparedStatement生成後イベントリスナ. */
	private final List<ExecutionConsumer<AfterCreatePreparedStatementEvent>> afterCreatePreparedStatementListeners = new ArrayList<>();
	/** CallableStatement生成後イベントリスナ. */
	private final List<ExecutionConsumer<AfterCreateCallableStatementEvent>> afterCreateCallableStatementListeners = new ArrayList<>();
	/** SQLQuery実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterSqlQueryEvent>> afterSqlQueryListeners = new ArrayList<>();
	/** SQLUpdate実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterSqlUpdateEvent>> afterSqlUpdateListeners = new ArrayList<>();
	/** SQLBatch実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterSqlBatchEvent>> afterSqlBatchListeners = new ArrayList<>();
	/** Procedure実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterProcedureEvent>> afterProcedureListeners = new ArrayList<>();
	/** EntityQuery実行前イベントリスナ. */
	private final List<ExecutionConsumer<BeforeEntityQueryEvent>> beforeEntityQueryListeners = new ArrayList<>();
	/** EntityQuery実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterEntityQueryEvent>> afterEntityQueryListeners = new ArrayList<>();
	/** EntityInsert実行前イベントリスナ. */
	private final List<ExecutionConsumer<BeforeEntityInsertEvent>> beforeEntityInsertListeners = new ArrayList<>();
	/** EntityInsert実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterEntityInsertEvent>> afterEntityInsertListeners = new ArrayList<>();
	/** EntityUpdate実行前イベントリスナ. */
	private final List<ExecutionConsumer<BeforeEntityUpdateEvent>> beforeEntityUpdateListeners = new ArrayList<>();
	/** EntityUpdate実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterEntityUpdateEvent>> afterEntityUpdateListeners = new ArrayList<>();
	/** EntityDelete実行前イベントリスナ. */
	private final List<ExecutionConsumer<BeforeEntityDeleteEvent>> beforeEntityDeleteListeners = new ArrayList<>();
	/** EntityDelete実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterEntityDeleteEvent>> afterEntityDeleteListeners = new ArrayList<>();
	/** EntityBatchInsert実行前イベントリスナ. */
	private final List<ExecutionConsumer<BeforeEntityBatchInsertEvent>> beforeEntityBatchInsertListeners = new ArrayList<>();
	/** EntityBatchInsert実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterEntityBatchInsertEvent>> afterEntityBatchInsertListeners = new ArrayList<>();
	/** EntityBatchUpdate実行前イベントリスナ. */
	private final List<ExecutionConsumer<BeforeEntityBatchUpdateEvent>> beforeEntityBatchUpdateListeners = new ArrayList<>();
	/** EntityBatchUpdate実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterEntityBatchUpdateEvent>> afterEntityBatchUpdateListeners = new ArrayList<>();
	/** EntityBulkInsert実行前イベントリスナ. */
	private final List<ExecutionConsumer<BeforeEntityBulkInsertEvent>> beforeEntityBulkInsertListeners = new ArrayList<>();
	/** EntityBulkInsert実行後イベントリスナ. */
	private final List<ExecutionConsumer<AfterEntityBulkInsertEvent>> afterEntityBulkInsertListeners = new ArrayList<>();
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
	 * EventListenerHolderに追加されたEventSubscriberのリストを取得する.
	 *
	 * @return EventSubscriberリスト
	 */
	public List<EventSubscriber> getEventSubscribers() {
		return List.copyOf(this.eventSubscribers);
	}

	/**
	 * EventSubscriberを追加し、EventSubscriberによるイベント登録を行う.
	 * @param eventSubscriber EventSubscriber
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addEventSubscriber(final EventSubscriber eventSubscriber) {
		this.eventSubscribers.add(eventSubscriber);
		eventSubscriber.initialize();
		eventSubscriber.subscribe(this);
		return this;
	}

	/**
	 * EventSubscriberを追加し、EventSubscriberによるイベント削除を行う.
	 * @param eventSubscriber EventSubscriber
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeEventSubscriber(final EventSubscriber eventSubscriber) {
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
	 * SQLパース前イベントリスナの追加
	 * @param listener SQLパース前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeParseSqlListener(final Consumer<BeforeParseSqlEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeParseSqlListeners.add(listener);
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
	public EventListenerHolder addAfterSqlQueryListener(final ExecutionConsumer<AfterSqlQueryEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSqlQueryListeners.add(listener);
		return this;
	}

	/**
	 * SQLUpdate実行後イベントリスナの追加
	 * @param listener SQLUpdate実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterSqlUpdateListener(final ExecutionConsumer<AfterSqlUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSqlUpdateListeners.add(listener);
		return this;
	}

	/**
	 * SQLBatch実行後イベントリスナの追加
	 * @param listener SQLBatch実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterSqlBatchListener(final ExecutionConsumer<AfterSqlBatchEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSqlBatchListeners.add(listener);
		return this;
	}

	/**
	 * Procedure実行後イベントリスナの追加
	 * @param listener Procedure実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterProcedureListener(final ExecutionConsumer<AfterProcedureEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterProcedureListeners.add(listener);
		return this;
	}

	/**
	 * EntityQuery実行前イベントリスナの追加
	 * @param listener EntityInsert実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeEntityQueryListener(
			final ExecutionConsumer<BeforeEntityQueryEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityQueryListeners.add(listener);
		return this;
	}

	/**
	 * EntityQuery実行後イベントリスナの追加
	 * @param listener EntityInsert実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterEntityQueryListener(
			final ExecutionConsumer<AfterEntityQueryEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityQueryListeners.add(listener);
		return this;
	}

	/**
	 * EntityInsert実行前イベントリスナの追加
	 * @param listener EntityInsert実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeEntityInsertListener(
			final ExecutionConsumer<BeforeEntityInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityInsertListeners.add(listener);
		return this;
	}

	/**
	 * EntityInsert実行後イベントリスナの追加
	 * @param listener EntityInsert実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterEntityInsertListener(
			final ExecutionConsumer<AfterEntityInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityInsertListeners.add(listener);
		return this;
	}

	/**
	 * EntityUpdate実行前イベントリスナの追加
	 * @param listener EntityUpdate実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeEntityUpdateListener(
			final ExecutionConsumer<BeforeEntityUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityUpdateListeners.add(listener);
		return this;
	}

	/**
	 * EntityUpdate実行後イベントリスナの追加
	 * @param listener EntityUpdate実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterEntityUpdateListener(
			final ExecutionConsumer<AfterEntityUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityUpdateListeners.add(listener);
		return this;
	}

	/**
	 * EntityDelete実行前イベントリスナの追加
	 * @param listener EntityDelete実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeEntityDeleteListener(
			final ExecutionConsumer<BeforeEntityDeleteEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityDeleteListeners.add(listener);
		return this;
	}

	/**
	 * EntityDelete実行後イベントリスナの追加
	 * @param listener EntityDelete実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterEntityDeleteListener(
			final ExecutionConsumer<AfterEntityDeleteEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityDeleteListeners.add(listener);
		return this;
	}

	/**
	 * EntityBatchInsert実行前イベントリスナの追加
	 * @param listener EntityBatchInsert実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeEntityBatchInsertListener(
			final ExecutionConsumer<BeforeEntityBatchInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityBatchInsertListeners.add(listener);
		return this;
	}

	/**
	 * EntityBatchInsert実行後イベントリスナの追加
	 * @param listener EntityBatchInsert実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterEntityBatchInsertListener(
			final ExecutionConsumer<AfterEntityBatchInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityBatchInsertListeners.add(listener);
		return this;
	}

	/**
	 * EntityBatchUpdate実行前イベントリスナの追加
	 * @param listener EntityBatchUpdate実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeEntityBatchUpdateListener(
			final ExecutionConsumer<BeforeEntityBatchUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityBatchUpdateListeners.add(listener);
		return this;
	}

	/**
	 * EntityBatchUpdate実行後イベントリスナの追加
	 * @param listener EntityBatchUpdate実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterEntityBatchUpdateListener(
			final ExecutionConsumer<AfterEntityBatchUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityBatchUpdateListeners.add(listener);
		return this;
	}

	/**
	 * EntityBulkInsert実行前イベントリスナの追加
	 * @param listener EntityBulkInsert実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addBeforeEntityBulkInsertListener(
			final ExecutionConsumer<BeforeEntityBulkInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityBulkInsertListeners.add(listener);
		return this;
	}

	/**
	 * EntityBulkInsert実行後イベントリスナの追加
	 * @param listener EntityBulkInsert実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder addAfterEntityBulkInsertListener(
			final ExecutionConsumer<AfterEntityBulkInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityBulkInsertListeners.add(listener);
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
	 * SQLパース前イベントリスナの削除
	 * @param listener SQLパース前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeParseSqlListener(final Consumer<BeforeParseSqlEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeParseSqlListeners.remove(listener);
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
	public EventListenerHolder removeAfterSqlQueryListener(final ExecutionConsumer<AfterSqlQueryEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSqlQueryListeners.remove(listener);
		return this;
	}

	/**
	 * SQLUpdate実行後イベントリスナの削除
	 * @param listener SQLUpdate実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterSqlUpdateListener(final ExecutionConsumer<AfterSqlUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSqlUpdateListeners.remove(listener);
		return this;
	}

	/**
	 * SQLBatch実行後イベントリスナの削除
	 * @param listener SQLBatch実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterSqlBatchListener(final ExecutionConsumer<AfterSqlBatchEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterSqlBatchListeners.remove(listener);
		return this;
	}

	/**
	 * Procedure実行後イベントリスナの削除
	 * @param listener Procedure実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterProcedureListener(final ExecutionConsumer<AfterProcedureEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterProcedureListeners.remove(listener);
		return this;
	}

	/**
	 * EntityQuery実行前イベントリスナの削除
	 * @param listener EntityQuery実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeEntityQueryListener(
			final ExecutionConsumer<BeforeEntityQueryEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityQueryListeners.remove(listener);
		return this;
	}

	/**
	 * EntityQuery実行後イベントリスナの削除
	 * @param listener EntityQuery実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterEntityQueryListener(
			final ExecutionConsumer<AfterEntityQueryEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityQueryListeners.remove(listener);
		return this;
	}

	/**
	 * EntityInsert実行前イベントリスナの削除
	 * @param listener EntityInsert実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeEntityInsertListener(
			final ExecutionConsumer<BeforeEntityInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityInsertListeners.remove(listener);
		return this;
	}

	/**
	 * EntityInsert実行後イベントリスナの削除
	 * @param listener EntityInsert実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterEntityInsertListener(
			final ExecutionConsumer<AfterEntityInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityInsertListeners.remove(listener);
		return this;
	}

	/**
	 * EntityUpdate実行前イベントリスナの削除
	 * @param listener EntityUpdate実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeEntityUpdateListener(
			final ExecutionConsumer<BeforeEntityUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityUpdateListeners.remove(listener);
		return this;
	}

	/**
	 * EntityUpdate実行後イベントリスナの削除
	 * @param listener EntityUpdate実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterEntityUpdateListener(
			final ExecutionConsumer<AfterEntityUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityUpdateListeners.remove(listener);
		return this;
	}

	/**
	 * EntityDelete実行前イベントリスナの削除
	 * @param listener EntityDelete実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeEntityDeleteListener(
			final ExecutionConsumer<BeforeEntityDeleteEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityDeleteListeners.remove(listener);
		return this;
	}

	/**
	 * EntityDelete実行後イベントリスナの削除
	 * @param listener EntityDelete実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterEntityDeleteListener(
			final ExecutionConsumer<AfterEntityDeleteEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityDeleteListeners.remove(listener);
		return this;
	}

	/**
	 * EntityBatchInsert実行前イベントリスナの削除
	 * @param listener EntityBatchInsert実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeEntityBatchInsertListener(
			final ExecutionConsumer<BeforeEntityBatchInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityBatchInsertListeners.remove(listener);
		return this;
	}

	/**
	 * EntityBatchInsert実行後イベントリスナの削除
	 * @param listener EntityBatchInsert実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterEntityBatchInsertListener(
			final ExecutionConsumer<AfterEntityBatchInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityBatchInsertListeners.remove(listener);
		return this;
	}

	/**
	 * EntityBatchUpdate実行前イベントリスナの削除
	 * @param listener EntityBatchUpdate実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeEntityBatchUpdateListener(
			final ExecutionConsumer<BeforeEntityBatchUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityBatchUpdateListeners.remove(listener);
		return this;
	}

	/**
	 * EntityBatchUpdate実行後イベントリスナの削除
	 * @param listener EntityBatchUpdate実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterEntityBatchUpdateListener(
			final ExecutionConsumer<AfterEntityBatchUpdateEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityBatchUpdateListeners.remove(listener);
		return this;
	}

	/**
	 * EntityBulkInsert実行前イベントリスナの削除
	 * @param listener EntityBulkInsert実行前イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeBeforeEntityBulkInsertListener(
			final ExecutionConsumer<BeforeEntityBulkInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.beforeEntityBulkInsertListeners.remove(listener);
		return this;
	}

	/**
	 * EntityBulkInsert実行後イベントリスナの削除
	 * @param listener EntityBulkInsert実行後イベントリスナ
	 * @return EventListenerHolder
	 */
	public EventListenerHolder removeAfterEntityBulkInsertListener(
			final ExecutionConsumer<AfterEntityBulkInsertEvent> listener) {
		Objects.requireNonNull(listener, "listener must not be null.");
		this.afterEntityBulkInsertListeners.remove(listener);
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
	 * SQLパース前イベントリスナリストの取得.
	 * @return SQLパース前イベントリスナリスト
	 */
	public List<Consumer<BeforeParseSqlEvent>> getBeforeParseSqlListeners() {
		return beforeParseSqlListeners;
	}

	/**
	 * 出力パラメータ取得後イベントリスナリストの取得.
	 * @return 出力パラメータ取得後イベントリスナリスト
	 */
	public List<Consumer<AfterGetOutParameterEvent>> getAfterGetOutParameterListeners() {
		return afterGetOutParameterListeners;
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
	public List<ExecutionConsumer<AfterSqlQueryEvent>> getAfterSqlQueryListeners() {
		return afterSqlQueryListeners;
	}

	/**
	 * SQLUpdate実行後イベントリスナリストの取得.
	 * @return SQLUpdate実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterSqlUpdateEvent>> getAfterSqlUpdateListeners() {
		return afterSqlUpdateListeners;
	}

	/**
	 * SQLBatch実行後イベントリスナリストの取得.
	 * @return SQLBatch実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterSqlBatchEvent>> getAfterSqlBatchListeners() {
		return afterSqlBatchListeners;
	}

	/**
	 * Procedure実行後イベントリスナリストの取得.
	 * @return Procedure実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterProcedureEvent>> getAfterProcedureListeners() {
		return afterProcedureListeners;
	}

	/**
	 * EntityQuery実行前イベントリスナリストの取得.
	 * @return EntityQuery実行前イベントリスナリスト
	 */
	public List<ExecutionConsumer<BeforeEntityQueryEvent>> getBeforeEntityQueryListeners() {
		return beforeEntityQueryListeners;
	}

	/**
	 * EntityQuery実行後イベントリスナリストの取得.
	 * @return EntityQuery実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterEntityQueryEvent>> getAfterEntityQueryListeners() {
		return afterEntityQueryListeners;
	}

	/**
	 * EntityInsert実行前イベントリスナリストの取得.
	 * @return EntityInsert実行前イベントリスナリスト
	 */
	public List<ExecutionConsumer<BeforeEntityInsertEvent>> getBeforeEntityInsertListeners() {
		return beforeEntityInsertListeners;
	}

	/**
	 * EntityInsert実行後イベントリスナリストの取得.
	 * @return EntityInsert実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterEntityInsertEvent>> getAfterEntityInsertListeners() {
		return afterEntityInsertListeners;
	}

	/**
	 * EntityUpdate実行前イベントリスナリストの取得.
	 * @return EntityUpdate実行前イベントリスナリスト
	 */
	public List<ExecutionConsumer<BeforeEntityUpdateEvent>> getBeforeEntityUpdateListeners() {
		return beforeEntityUpdateListeners;
	}

	/**
	 * EntityUpdate実行後イベントリスナリストの取得.
	 * @return EntityUpdate実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterEntityUpdateEvent>> getAfterEntityUpdateListeners() {
		return afterEntityUpdateListeners;
	}

	/**
	 * EntityDelete実行前イベントリスナリストの取得.
	 * @return EntityDelete実行前イベントリスナリスト
	 */
	public List<ExecutionConsumer<BeforeEntityDeleteEvent>> getBeforeEntityDeleteListeners() {
		return beforeEntityDeleteListeners;
	}

	/**
	 * EntityDelete実行後イベントリスナリストの取得.
	 * @return EntityDelete実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterEntityDeleteEvent>> getAfterEntityDeleteListeners() {
		return afterEntityDeleteListeners;
	}

	/**
	 * EntityBatchInsert実行前イベントリスナリストの取得.
	 * @return EntityBatchInsert実行前イベントリスナリスト
	 */
	public List<ExecutionConsumer<BeforeEntityBatchInsertEvent>> getBeforeEntityBatchInsertListeners() {
		return beforeEntityBatchInsertListeners;
	}

	/**
	 * EntityBatchInsert実行後イベントリスナリストの取得.
	 * @return EntityBatchInsert実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterEntityBatchInsertEvent>> getAfterEntityBatchInsertListeners() {
		return afterEntityBatchInsertListeners;
	}

	/**
	 * EntityBatchUpdate実行前イベントリスナリストの取得.
	 * @return EntityBatchUpdate実行前イベントリスナリスト
	 */
	public List<ExecutionConsumer<BeforeEntityBatchUpdateEvent>> getBeforeEntityBatchUpdateListeners() {
		return beforeEntityBatchUpdateListeners;
	}

	/**
	 * EntityBatchUpdate実行後イベントリスナリストの取得.
	 * @return EntityBatchUpdate実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterEntityBatchUpdateEvent>> getAfterEntityBatchUpdateListeners() {
		return afterEntityBatchUpdateListeners;
	}

	/**
	 * EntityBulkInsert実行前イベントリスナリストの取得.
	 * @return EntityBulkInsert実行前イベントリスナリスト
	 */
	public List<ExecutionConsumer<BeforeEntityBulkInsertEvent>> getBeforeEntityBulkInsertListeners() {
		return beforeEntityBulkInsertListeners;
	}

	/**
	 * EntityBulkInsert実行後イベントリスナリストの取得.
	 * @return EntityBulkInsert実行後イベントリスナリスト
	 */
	public List<ExecutionConsumer<AfterEntityBulkInsertEvent>> getAfterEntityBulkInsertListeners() {
		return afterEntityBulkInsertListeners;
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
	 * SQLパース前イベントリスナがあるかどうか.
	 * @return SQLパース前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeParseSqlListener() {
		return !beforeParseSqlListeners.isEmpty();
	}

	/**
	 * 出力パラメータ取得後イベントリスナがあるかどうか.
	 * @return 出力パラメータ取得後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterGetOutParameterListener() {
		return !afterGetOutParameterListeners.isEmpty();
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
	public boolean hasAfterSqlQueryListener() {
		return !afterSqlQueryListeners.isEmpty();
	}

	/**
	 * SQLUpdate実行後イベントリスナがあるかどうか.
	 * @return SQLUpdate実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterSqlUpdateListener() {
		return !afterSqlUpdateListeners.isEmpty();
	}

	/**
	 * SQLBatch実行後イベントリスナがあるかどうか.
	 * @return SQLBatch実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterSqlBatchListener() {
		return !afterSqlBatchListeners.isEmpty();
	}

	/**
	 * Procedure実行後イベントリスナがあるかどうか.
	 * @return Procedure実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterProcedureListener() {
		return !afterProcedureListeners.isEmpty();
	}

	/**
	 * EntityQuery実行前イベントリスナがあるかどうか.
	 * @return EntityQuery実行前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeEntityQueryListener() {
		return !beforeEntityQueryListeners.isEmpty();
	}

	/**
	 * EntityQuery実行後イベントリスナがあるかどうか.
	 * @return EntityQuery実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterEntityQueryListener() {
		return !afterEntityQueryListeners.isEmpty();
	}

	/**
	 * EntityInsert実行前イベントリスナがあるかどうか.
	 * @return EntityInsert実行前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeEntityInsertListener() {
		return !beforeEntityInsertListeners.isEmpty();
	}

	/**
	 * EntityInsert実行後イベントリスナがあるかどうか.
	 * @return EntityInsert実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterEntityInsertListener() {
		return !afterEntityInsertListeners.isEmpty();
	}

	/**
	 * EntityUpdate実行前イベントリスナがあるかどうか.
	 * @return EntityUpdate実行前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeEntityUpdateListener() {
		return !beforeEntityUpdateListeners.isEmpty();
	}

	/**
	 * EntityUpdate実行後イベントリスナがあるかどうか.
	 * @return EntityUpdate実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterEntityUpdateListener() {
		return !afterEntityUpdateListeners.isEmpty();
	}

	/**
	 * EntityDelete実行前イベントリスナがあるかどうか.
	 * @return EntityDelete実行前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeEntityDeleteListener() {
		return !beforeEntityDeleteListeners.isEmpty();
	}

	/**
	 * EntityDelete実行後イベントリスナがあるかどうか.
	 * @return EntityDelete実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterEntityDeleteListener() {
		return !afterEntityDeleteListeners.isEmpty();
	}

	/**
	 * EntityBatchInsert実行前イベントリスナがあるかどうか.
	 * @return EntityBatchInsert実行前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeEntityBatchInsertListener() {
		return !beforeEntityBatchInsertListeners.isEmpty();
	}

	/**
	 * EntityBatchInsert実行後イベントリスナがあるかどうか.
	 * @return EntityBatchInsert実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterEntityBatchInsertListener() {
		return !afterEntityBatchInsertListeners.isEmpty();
	}

	/**
	 * EntityBatchUpdate実行前イベントリスナがあるかどうか.
	 * @return EntityBatchUpdate実行前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeEntityBatchUpdateListener() {
		return !beforeEntityBatchUpdateListeners.isEmpty();
	}

	/**
	 * EntityBatchUpdate実行後イベントリスナがあるかどうか.
	 * @return EntityBatchUpdate実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterEntityBatchUpdateListener() {
		return !afterEntityBatchUpdateListeners.isEmpty();
	}

	/**
	 * EntityBulkInsert実行前イベントリスナがあるかどうか.
	 * @return EntityBulkInsert実行前イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasBeforeEntityBulkInsertListener() {
		return !beforeEntityBulkInsertListeners.isEmpty();
	}

	/**
	 * EntityBulkInsert実行後イベントリスナがあるかどうか.
	 * @return EntityBulkInsert実行後イベントリスナがある場合は<code>true</code>
	 */
	public boolean hasAfterEntityBulkInsertListener() {
		return !afterEntityBulkInsertListeners.isEmpty();
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
