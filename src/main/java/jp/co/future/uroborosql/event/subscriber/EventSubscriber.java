/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event.subscriber;

import java.util.Optional;
import java.util.function.Consumer;

import jp.co.future.uroborosql.event.AfterBeginTransactionEvent;
import jp.co.future.uroborosql.event.AfterCommitEvent;
import jp.co.future.uroborosql.event.AfterCreateCallableStatementEvent;
import jp.co.future.uroborosql.event.AfterCreatePreparedStatementEvent;
import jp.co.future.uroborosql.event.AfterGetOutParameterEvent;
import jp.co.future.uroborosql.event.AfterInitializeExecutionContextEvent;
import jp.co.future.uroborosql.event.AfterRollbackEvent;
import jp.co.future.uroborosql.event.BeforeCommitEvent;
import jp.co.future.uroborosql.event.BeforeEndTransactionEvent;
import jp.co.future.uroborosql.event.BeforeParseSqlEvent;
import jp.co.future.uroborosql.event.BeforeRollbackEvent;
import jp.co.future.uroborosql.event.BeforeSetParameterEvent;
import jp.co.future.uroborosql.event.EventListenerHolder;
import jp.co.future.uroborosql.event.ExecutionConsumer;
import jp.co.future.uroborosql.event.ProcedureEvent;
import jp.co.future.uroborosql.event.SqlBatchEvent;
import jp.co.future.uroborosql.event.SqlQueryEvent;
import jp.co.future.uroborosql.event.SqlUpdateEvent;
import jp.co.future.uroborosql.event.TransformSqlEvent;

public abstract class EventSubscriber {
	/** ExecutionContext初期化後イベントリスナ. */
	private Optional<Consumer<AfterInitializeExecutionContextEvent>> afterInitializeExecutionContextListener = Optional
			.empty();
	/** パラメータ設定前イベントリスナ. */
	private Optional<Consumer<BeforeSetParameterEvent>> beforeSetParameterListener = Optional.empty();
	/** SQL変換イベントリスナ. */
	private Optional<Consumer<TransformSqlEvent>> transformSqlListener = Optional.empty();
	/** SQLパース前イベントリスナ. */
	private Optional<Consumer<BeforeParseSqlEvent>> beforeParseSqlListener = Optional.empty();
	/** 出力パラメータ取得後イベントリスナ. */
	private Optional<Consumer<AfterGetOutParameterEvent>> afterGetOutParameterListener = Optional.empty();
	/** PreparedStatement生成後イベントリスナ. */
	private Optional<ExecutionConsumer<AfterCreatePreparedStatementEvent>> afterCreatePreparedStatementListener = Optional
			.empty();
	/** CallableStatement生成後イベントリスナ. */
	private Optional<ExecutionConsumer<AfterCreateCallableStatementEvent>> afterCreateCallableStatementListener = Optional
			.empty();
	/** SQLQuery実行後イベントリスナ. */
	private Optional<ExecutionConsumer<SqlQueryEvent>> sqlQueryListener = Optional.empty();
	/** SQLUpdate実行後イベントリスナ. */
	private Optional<ExecutionConsumer<SqlUpdateEvent>> sqlUpdateListener = Optional.empty();
	/** SQLBatch実行後イベントリスナ. */
	private Optional<ExecutionConsumer<SqlBatchEvent>> sqlBatchListener = Optional.empty();
	/** Procedure実行後イベントリスナ. */
	private Optional<ExecutionConsumer<ProcedureEvent>> procedureListener = Optional.empty();
	/** トランザクション開始後イベントリスナ. */
	private Optional<Consumer<AfterBeginTransactionEvent>> afterBeginTransactionListener = Optional.empty();
	/** トランザクション終了前イベントリスナ. */
	private Optional<Consumer<BeforeEndTransactionEvent>> beforeEndTransactionListener = Optional.empty();
	/** コミット前イベントリスナ. */
	private Optional<Consumer<BeforeCommitEvent>> beforeCommitListener = Optional.empty();
	/** コミット後イベントリスナ. */
	private Optional<Consumer<AfterCommitEvent>> afterCommitListener = Optional.empty();
	/** ロールバック前イベントリスナ. */
	private Optional<Consumer<BeforeRollbackEvent>> beforeRollbackListener = Optional.empty();
	/** ロールバック後イベントリスナ. */
	private Optional<Consumer<AfterRollbackEvent>> afterRollbackListener = Optional.empty();

	/**
	 * 初期化処理.
	 */
	public abstract void initialize();

	/**
	 * イベントの購読を行う.
	 *
	 * @param eventListenerHolder EventListenerHolder
	 */
	public final void subscribe(EventListenerHolder eventListenerHolder) {
		afterInitializeExecutionContextListener()
				.ifPresent(listener -> eventListenerHolder.addAfterInitializeExecutionContextListener(listener));
		beforeSetParameterListener().ifPresent(listener -> eventListenerHolder.addBeforeSetParameterListener(listener));
		transformSqlListener().ifPresent(listener -> eventListenerHolder.addTransformSqlListener(listener));
		beforeParseSqlListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeParseSqlListener(listener));
		afterGetOutParameterListener()
				.ifPresent(listener -> eventListenerHolder.addAfterGetOutParameterListener(listener));
		afterCreatePreparedStatementListener()
				.ifPresent(listener -> eventListenerHolder.addAfterCreatePreparedStatementListener(listener));
		afterCreateCallableStatementListener()
				.ifPresent(listener -> eventListenerHolder.addAfterCreateCallableStatementListener(listener));
		sqlQueryListener().ifPresent(listener -> eventListenerHolder.addSqlQueryListener(listener));
		sqlUpdateListener().ifPresent(listener -> eventListenerHolder.addSqlUpdateListener(listener));
		sqlBatchListener().ifPresent(listener -> eventListenerHolder.addSqlBatchListener(listener));
		procedureListener().ifPresent(listener -> eventListenerHolder.addProcedureListener(listener));
		afterBeginTransactionListener()
				.ifPresent(listener -> eventListenerHolder.addAfterBeginTransactionListener(listener));
		beforeEndTransactionListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeEndTransactionListener(listener));
		beforeCommitListener().ifPresent(listener -> eventListenerHolder.addBeforeCommitListener(listener));
		afterCommitListener().ifPresent(listener -> eventListenerHolder.addAfterCommitListener(listener));
		beforeRollbackListener().ifPresent(listener -> eventListenerHolder.addBeforeRollbackListener(listener));
		afterRollbackListener().ifPresent(listener -> eventListenerHolder.addAfterRollbackListener(listener));
	}

	/**
	 * イベントの購読解除を行う.
	 *
	 * @param eventListenerHolder EventListenerHolder
	 */
	public final void unsubscribe(EventListenerHolder eventListenerHolder) {
		afterInitializeExecutionContextListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterInitializeExecutionContextListener(listener));
		beforeSetParameterListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeSetParameterListener(listener));
		transformSqlListener()
				.ifPresent(listener -> eventListenerHolder.removeTransformSqlListener(listener));
		beforeParseSqlListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeParseSqlListener(listener));
		afterGetOutParameterListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterGetOutParameterListener(listener));
		afterCreatePreparedStatementListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterCreatePreparedStatementListener(listener));
		afterCreateCallableStatementListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterCreateCallableStatementListener(listener));
		sqlQueryListener().ifPresent(listener -> eventListenerHolder.removeSqlQueryListener(listener));
		sqlUpdateListener().ifPresent(listener -> eventListenerHolder.removeSqlUpdateListener(listener));
		sqlBatchListener().ifPresent(listener -> eventListenerHolder.removeSqlBatchListener(listener));
		procedureListener().ifPresent(listener -> eventListenerHolder.removeProcedureListener(listener));
		afterBeginTransactionListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterBeginTransactionListener(listener));
		beforeEndTransactionListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeEndTransactionListener(listener));
		beforeCommitListener().ifPresent(listener -> eventListenerHolder.removeBeforeCommitListener(listener));
		afterCommitListener().ifPresent(listener -> eventListenerHolder.removeAfterCommitListener(listener));
		beforeRollbackListener().ifPresent(listener -> eventListenerHolder.removeBeforeRollbackListener(listener));
		afterRollbackListener().ifPresent(listener -> eventListenerHolder.removeAfterRollbackListener(listener));
	}

	protected final Optional<Consumer<AfterInitializeExecutionContextEvent>> afterInitializeExecutionContextListener() {
		return afterInitializeExecutionContextListener;
	}

	protected final Optional<Consumer<BeforeSetParameterEvent>> beforeSetParameterListener() {
		return beforeSetParameterListener;
	}

	protected final Optional<Consumer<TransformSqlEvent>> transformSqlListener() {
		return transformSqlListener;
	}

	protected final Optional<Consumer<BeforeParseSqlEvent>> beforeParseSqlListener() {
		return beforeParseSqlListener;
	}

	protected final Optional<Consumer<AfterGetOutParameterEvent>> afterGetOutParameterListener() {
		return afterGetOutParameterListener;
	}

	protected final Optional<ExecutionConsumer<AfterCreatePreparedStatementEvent>> afterCreatePreparedStatementListener() {
		return afterCreatePreparedStatementListener;
	}

	protected final Optional<ExecutionConsumer<AfterCreateCallableStatementEvent>> afterCreateCallableStatementListener() {
		return afterCreateCallableStatementListener;
	}

	protected final Optional<ExecutionConsumer<SqlQueryEvent>> sqlQueryListener() {
		return sqlQueryListener;
	}

	protected final Optional<ExecutionConsumer<SqlUpdateEvent>> sqlUpdateListener() {
		return sqlUpdateListener;
	}

	protected final Optional<ExecutionConsumer<SqlBatchEvent>> sqlBatchListener() {
		return sqlBatchListener;
	}

	protected final Optional<ExecutionConsumer<ProcedureEvent>> procedureListener() {
		return procedureListener;
	}

	protected final Optional<Consumer<AfterBeginTransactionEvent>> afterBeginTransactionListener() {
		return afterBeginTransactionListener;
	}

	protected final Optional<Consumer<BeforeEndTransactionEvent>> beforeEndTransactionListener() {
		return beforeEndTransactionListener;
	}

	protected final Optional<Consumer<BeforeCommitEvent>> beforeCommitListener() {
		return beforeCommitListener;
	}

	protected final Optional<Consumer<AfterCommitEvent>> afterCommitListener() {
		return afterCommitListener;
	}

	protected final Optional<Consumer<BeforeRollbackEvent>> beforeRollbackListener() {
		return beforeRollbackListener;
	}

	protected final Optional<Consumer<AfterRollbackEvent>> afterRollbackListener() {
		return afterRollbackListener;
	}

	protected final void afterInitializeExecutionContextListener(
			Consumer<AfterInitializeExecutionContextEvent> listener) {
		this.afterInitializeExecutionContextListener = Optional.ofNullable(listener);
	}

	protected final void beforeSetParameterListener(Consumer<BeforeSetParameterEvent> listener) {
		this.beforeSetParameterListener = Optional.ofNullable(listener);
	}

	protected final void transformSqlListener(Consumer<TransformSqlEvent> listener) {
		this.transformSqlListener = Optional.ofNullable(listener);
	}

	protected final void beforeParseSqlListener(Consumer<BeforeParseSqlEvent> listener) {
		this.beforeParseSqlListener = Optional.ofNullable(listener);
	}

	protected final void afterGetOutParameterListener(Consumer<AfterGetOutParameterEvent> listener) {
		this.afterGetOutParameterListener = Optional.ofNullable(listener);
	}

	protected final void afterCreatePreparedStatementListener(
			ExecutionConsumer<AfterCreatePreparedStatementEvent> listener) {
		this.afterCreatePreparedStatementListener = Optional.ofNullable(listener);
	}

	protected final void afterCreateCallableStatementListener(
			ExecutionConsumer<AfterCreateCallableStatementEvent> listener) {
		this.afterCreateCallableStatementListener = Optional.ofNullable(listener);
	}

	protected final void sqlQueryListener(ExecutionConsumer<SqlQueryEvent> listener) {
		this.sqlQueryListener = Optional.ofNullable(listener);
	}

	protected final void sqlUpdateListener(ExecutionConsumer<SqlUpdateEvent> listener) {
		this.sqlUpdateListener = Optional.ofNullable(listener);
	}

	protected final void sqlBatchListener(ExecutionConsumer<SqlBatchEvent> listener) {
		this.sqlBatchListener = Optional.ofNullable(listener);
	}

	protected final void procedureListener(ExecutionConsumer<ProcedureEvent> listener) {
		this.procedureListener = Optional.ofNullable(listener);
	}

	protected final void afterBeginTransactionListener(Consumer<AfterBeginTransactionEvent> listener) {
		this.afterBeginTransactionListener = Optional.ofNullable(listener);
	}

	protected final void beforeEndTransactionListener(Consumer<BeforeEndTransactionEvent> listener) {
		this.beforeEndTransactionListener = Optional.ofNullable(listener);
	}

	protected final void beforeCommitListener(Consumer<BeforeCommitEvent> listener) {
		this.beforeCommitListener = Optional.ofNullable(listener);
	}

	protected final void afterCommitListener(Consumer<AfterCommitEvent> listener) {
		this.afterCommitListener = Optional.ofNullable(listener);
	}

	protected final void beforeRollbackListener(Consumer<BeforeRollbackEvent> listener) {
		this.beforeRollbackListener = Optional.ofNullable(listener);
	}

	protected final void afterRollbackListener(Consumer<AfterRollbackEvent> listener) {
		this.afterRollbackListener = Optional.ofNullable(listener);
	}

}
