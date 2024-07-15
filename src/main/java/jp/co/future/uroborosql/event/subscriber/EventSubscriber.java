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
import jp.co.future.uroborosql.event.AfterEntityBatchInsertEvent;
import jp.co.future.uroborosql.event.AfterEntityBatchUpdateEvent;
import jp.co.future.uroborosql.event.AfterEntityBulkInsertEvent;
import jp.co.future.uroborosql.event.AfterEntityDeleteEvent;
import jp.co.future.uroborosql.event.AfterEntityInsertEvent;
import jp.co.future.uroborosql.event.AfterEntityQueryEvent;
import jp.co.future.uroborosql.event.AfterEntityUpdateEvent;
import jp.co.future.uroborosql.event.AfterGetOutParameterEvent;
import jp.co.future.uroborosql.event.AfterInitializeExecutionContextEvent;
import jp.co.future.uroborosql.event.AfterRollbackEvent;
import jp.co.future.uroborosql.event.BeforeCommitEvent;
import jp.co.future.uroborosql.event.BeforeEndTransactionEvent;
import jp.co.future.uroborosql.event.BeforeEntityBatchInsertEvent;
import jp.co.future.uroborosql.event.BeforeEntityBatchUpdateEvent;
import jp.co.future.uroborosql.event.BeforeEntityBulkInsertEvent;
import jp.co.future.uroborosql.event.BeforeEntityDeleteEvent;
import jp.co.future.uroborosql.event.BeforeEntityInsertEvent;
import jp.co.future.uroborosql.event.BeforeEntityQueryEvent;
import jp.co.future.uroborosql.event.BeforeEntityUpdateEvent;
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

/**
 * イベントサブスクライバ抽象親クラス
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
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
	/** EntityQuery実行前イベントリスナ. */
	private Optional<ExecutionConsumer<BeforeEntityQueryEvent>> beforeEntityQueryListener = Optional.empty();
	/** EntityQuery実行後イベントリスナ. */
	private Optional<ExecutionConsumer<AfterEntityQueryEvent>> afterEntityQueryListener = Optional.empty();
	/** EntityInsert実行前イベントリスナ. */
	private Optional<ExecutionConsumer<BeforeEntityInsertEvent>> beforeEntityInsertListener = Optional.empty();
	/** EntityInsert実行後イベントリスナ. */
	private Optional<ExecutionConsumer<AfterEntityInsertEvent>> afterEntityInsertListener = Optional.empty();
	/** EntityUpdate実行前イベントリスナ. */
	private Optional<ExecutionConsumer<BeforeEntityUpdateEvent>> beforeEntityUpdateListener = Optional.empty();
	/** EntityUpdate実行後イベントリスナ. */
	private Optional<ExecutionConsumer<AfterEntityUpdateEvent>> afterEntityUpdateListener = Optional.empty();
	/** EntityDelete実行前イベントリスナ. */
	private Optional<ExecutionConsumer<BeforeEntityDeleteEvent>> beforeEntityDeleteListener = Optional.empty();
	/** EntityDelete実行後イベントリスナ. */
	private Optional<ExecutionConsumer<AfterEntityDeleteEvent>> afterEntityDeleteListener = Optional.empty();
	/** EntityBatchInsert実行前イベントリスナ. */
	private Optional<ExecutionConsumer<BeforeEntityBatchInsertEvent>> beforeEntityBatchInsertListener = Optional
			.empty();
	/** EntityBatchInsert実行後イベントリスナ. */
	private Optional<ExecutionConsumer<AfterEntityBatchInsertEvent>> afterEntityBatchInsertListener = Optional.empty();
	/** EntityBatchUpdate実行前イベントリスナ. */
	private Optional<ExecutionConsumer<BeforeEntityBatchUpdateEvent>> beforeEntityBatchUpdateListener = Optional
			.empty();
	/** EntityBatchUpdate実行後イベントリスナ. */
	private Optional<ExecutionConsumer<AfterEntityBatchUpdateEvent>> afterEntityBatchUpdateListener = Optional.empty();
	/** EntityBulkInsert実行前イベントリスナ. */
	private Optional<ExecutionConsumer<BeforeEntityBulkInsertEvent>> beforeEntityBulkInsertListener = Optional.empty();
	/** EntityBulkInsert実行後イベントリスナ. */
	private Optional<ExecutionConsumer<AfterEntityBulkInsertEvent>> afterEntityBulkInsertListener = Optional.empty();

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
	public final void subscribe(final EventListenerHolder eventListenerHolder) {
		/** ExecutionContext初期化後イベントリスナ. */
		afterInitializeExecutionContextListener()
				.ifPresent(listener -> eventListenerHolder.addAfterInitializeExecutionContextListener(listener));
		/** パラメータ設定前イベントリスナ. */
		beforeSetParameterListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeSetParameterListener(listener));
		/** SQL変換イベントリスナ. */
		transformSqlListener()
				.ifPresent(listener -> eventListenerHolder.addTransformSqlListener(listener));
		/** SQLパース前イベントリスナ. */
		beforeParseSqlListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeParseSqlListener(listener));
		/** 出力パラメータ取得後イベントリスナ. */
		afterGetOutParameterListener()
				.ifPresent(listener -> eventListenerHolder.addAfterGetOutParameterListener(listener));
		/** PreparedStatement生成後イベントリスナ. */
		afterCreatePreparedStatementListener()
				.ifPresent(listener -> eventListenerHolder.addAfterCreatePreparedStatementListener(listener));
		/** CallableStatement生成後イベントリスナ. */
		afterCreateCallableStatementListener()
				.ifPresent(listener -> eventListenerHolder.addAfterCreateCallableStatementListener(listener));
		/** SQLQuery実行後イベントリスナ. */
		sqlQueryListener()
				.ifPresent(listener -> eventListenerHolder.addSqlQueryListener(listener));
		/** SQLUpdate実行後イベントリスナ. */
		sqlUpdateListener()
				.ifPresent(listener -> eventListenerHolder.addSqlUpdateListener(listener));
		/** SQLBatch実行後イベントリスナ. */
		sqlBatchListener()
				.ifPresent(listener -> eventListenerHolder.addSqlBatchListener(listener));
		/** Procedure実行後イベントリスナ. */
		procedureListener()
				.ifPresent(listener -> eventListenerHolder.addProcedureListener(listener));
		/** EntityQuery実行前イベントリスナ. */
		beforeEntityQueryListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeEntityQueryListener(listener));
		/** EntityQuery実行後イベントリスナ. */
		afterEntityQueryListener()
				.ifPresent(listener -> eventListenerHolder.addAfterEntityQueryListener(listener));
		/** EntityInsert実行前イベントリスナ. */
		beforeEntityInsertListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeEntityInsertListener(listener));
		/** EntityInsert実行後イベントリスナ. */
		afterEntityInsertListener()
				.ifPresent(listener -> eventListenerHolder.addAfterEntityInsertListener(listener));
		/** EntityUpdate実行前イベントリスナ. */
		beforeEntityUpdateListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeEntityUpdateListener(listener));
		/** EntityUpdate実行後イベントリスナ. */
		afterEntityUpdateListener()
				.ifPresent(listener -> eventListenerHolder.addAfterEntityUpdateListener(listener));
		/** EntityDelete実行前イベントリスナ. */
		beforeEntityDeleteListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeEntityDeleteListener(listener));
		/** EntityDelete実行後イベントリスナ. */
		afterEntityDeleteListener()
				.ifPresent(listener -> eventListenerHolder.addAfterEntityDeleteListener(listener));
		/** EntityBatchInsert実行前イベントリスナ. */
		beforeEntityBatchInsertListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeEntityBatchInsertListener(listener));
		/** EntityBatchInsert実行後イベントリスナ. */
		afterEntityBatchInsertListener()
				.ifPresent(listener -> eventListenerHolder.addAfterEntityBatchInsertListener(listener));
		/** EntityBatchUpdate実行前イベントリスナ. */
		beforeEntityBatchUpdateListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeEntityBatchUpdateListener(listener));
		/** EntityBatchUpdate実行後イベントリスナ. */
		afterEntityBatchUpdateListener()
				.ifPresent(listener -> eventListenerHolder.addAfterEntityBatchUpdateListener(listener));
		/** EntityBulkInsert実行前イベントリスナ. */
		beforeEntityBulkInsertListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeEntityBulkInsertListener(listener));
		/** EntityBulkInsert実行後イベントリスナ. */
		afterEntityBulkInsertListener()
				.ifPresent(listener -> eventListenerHolder.addAfterEntityBulkInsertListener(listener));
		/** トランザクション開始後イベントリスナ. */
		afterBeginTransactionListener()
				.ifPresent(listener -> eventListenerHolder.addAfterBeginTransactionListener(listener));
		/** トランザクション終了前イベントリスナ. */
		beforeEndTransactionListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeEndTransactionListener(listener));
		/** コミット前イベントリスナ. */
		beforeCommitListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeCommitListener(listener));
		/** コミット後イベントリスナ. */
		afterCommitListener()
				.ifPresent(listener -> eventListenerHolder.addAfterCommitListener(listener));
		/** ロールバック前イベントリスナ. */
		beforeRollbackListener()
				.ifPresent(listener -> eventListenerHolder.addBeforeRollbackListener(listener));
		/** ロールバック後イベントリスナ. */
		afterRollbackListener()
				.ifPresent(listener -> eventListenerHolder.addAfterRollbackListener(listener));
	}

	/**
	 * イベントの購読解除を行う.
	 *
	 * @param eventListenerHolder EventListenerHolder
	 */
	public final void unsubscribe(final EventListenerHolder eventListenerHolder) {
		/** ExecutionContext初期化後イベントリスナ. */
		afterInitializeExecutionContextListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterInitializeExecutionContextListener(listener));
		/** パラメータ設定前イベントリスナ. */
		beforeSetParameterListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeSetParameterListener(listener));
		/** SQL変換イベントリスナ. */
		transformSqlListener()
				.ifPresent(listener -> eventListenerHolder.removeTransformSqlListener(listener));
		/** SQLパース前イベントリスナ. */
		beforeParseSqlListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeParseSqlListener(listener));
		/** 出力パラメータ取得後イベントリスナ. */
		afterGetOutParameterListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterGetOutParameterListener(listener));
		/** PreparedStatement生成後イベントリスナ. */
		afterCreatePreparedStatementListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterCreatePreparedStatementListener(listener));
		/** CallableStatement生成後イベントリスナ. */
		afterCreateCallableStatementListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterCreateCallableStatementListener(listener));
		/** SQLQuery実行後イベントリスナ. */
		sqlQueryListener()
				.ifPresent(listener -> eventListenerHolder.removeSqlQueryListener(listener));
		/** SQLUpdate実行後イベントリスナ. */
		sqlUpdateListener()
				.ifPresent(listener -> eventListenerHolder.removeSqlUpdateListener(listener));
		/** SQLBatch実行後イベントリスナ. */
		sqlBatchListener()
				.ifPresent(listener -> eventListenerHolder.removeSqlBatchListener(listener));
		/** Procedure実行後イベントリスナ. */
		procedureListener()
				.ifPresent(listener -> eventListenerHolder.removeProcedureListener(listener));
		/** EntityQuery実行前イベントリスナ. */
		beforeEntityQueryListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeEntityQueryListener(listener));
		/** EntityQuery実行後イベントリスナ. */
		afterEntityQueryListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterEntityQueryListener(listener));
		/** EntityInsert実行前イベントリスナ. */
		beforeEntityInsertListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeEntityInsertListener(listener));
		/** EntityInsert実行後イベントリスナ. */
		afterEntityInsertListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterEntityInsertListener(listener));
		/** EntityUpdate実行前イベントリスナ. */
		beforeEntityUpdateListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeEntityUpdateListener(listener));
		/** EntityUpdate実行後イベントリスナ. */
		afterEntityUpdateListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterEntityUpdateListener(listener));
		/** EntityDelete実行前イベントリスナ. */
		beforeEntityDeleteListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeEntityDeleteListener(listener));
		/** EntityDelete実行後イベントリスナ. */
		afterEntityDeleteListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterEntityDeleteListener(listener));
		/** EntityBatchInsert実行前イベントリスナ. */
		beforeEntityBatchInsertListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeEntityBatchInsertListener(listener));
		/** EntityBatchInsert実行後イベントリスナ. */
		afterEntityBatchInsertListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterEntityBatchInsertListener(listener));
		/** EntityBatchUpdate実行前イベントリスナ. */
		beforeEntityBatchUpdateListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeEntityBatchUpdateListener(listener));
		/** EntityBatchUpdate実行後イベントリスナ. */
		afterEntityBatchUpdateListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterEntityBatchUpdateListener(listener));
		/** EntityBulkInsert実行前イベントリスナ. */
		beforeEntityBulkInsertListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeEntityBulkInsertListener(listener));
		/** EntityBulkInsert実行後イベントリスナ. */
		afterEntityBulkInsertListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterEntityBulkInsertListener(listener));
		/** トランザクション開始後イベントリスナ. */
		afterBeginTransactionListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterBeginTransactionListener(listener));
		/** トランザクション終了前イベントリスナ. */
		beforeEndTransactionListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeEndTransactionListener(listener));
		/** コミット前イベントリスナ. */
		beforeCommitListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeCommitListener(listener));
		/** コミット後イベントリスナ. */
		afterCommitListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterCommitListener(listener));
		/** ロールバック前イベントリスナ. */
		beforeRollbackListener()
				.ifPresent(listener -> eventListenerHolder.removeBeforeRollbackListener(listener));
		/** ロールバック後イベントリスナ. */
		afterRollbackListener()
				.ifPresent(listener -> eventListenerHolder.removeAfterRollbackListener(listener));
	}

	/**
	 * ExecutionContext初期化後イベントリスナ の取得
	 * @return ExecutionContext初期化後イベントリスナ
	 */
	protected final Optional<Consumer<AfterInitializeExecutionContextEvent>> afterInitializeExecutionContextListener() {
		return afterInitializeExecutionContextListener;
	}

	/**
	 * パラメータ設定前イベントリスナ の取得
	 * @return パラメータ設定前イベントリスナ
	 */
	protected final Optional<Consumer<BeforeSetParameterEvent>> beforeSetParameterListener() {
		return beforeSetParameterListener;
	}

	/**
	 * SQL変換イベントリスナ の取得
	 * @return SQL変換イベントリスナ
	 */
	protected final Optional<Consumer<TransformSqlEvent>> transformSqlListener() {
		return transformSqlListener;
	}

	/**
	 * SQLパース前イベントリスナ の取得
	 * @return SQLパース前イベントリスナ
	 */
	protected final Optional<Consumer<BeforeParseSqlEvent>> beforeParseSqlListener() {
		return beforeParseSqlListener;
	}

	/**
	 * 出力パラメータ取得後イベントリスナ の取得
	 * @return 出力パラメータ取得後イベントリスナ
	 */
	protected final Optional<Consumer<AfterGetOutParameterEvent>> afterGetOutParameterListener() {
		return afterGetOutParameterListener;
	}

	/**
	 * PreparedStatement生成後イベントリスナ の取得
	 * @return PreparedStatement生成後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<AfterCreatePreparedStatementEvent>> afterCreatePreparedStatementListener() {
		return afterCreatePreparedStatementListener;
	}

	/**
	 * CallableStatement生成後イベントリスナ の取得
	 * @return CallableStatement生成後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<AfterCreateCallableStatementEvent>> afterCreateCallableStatementListener() {
		return afterCreateCallableStatementListener;
	}

	/**
	 * SQLQuery実行後イベントリスナ の取得
	 * @return SQLQuery実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<SqlQueryEvent>> sqlQueryListener() {
		return sqlQueryListener;
	}

	/**
	 * SQLUpdate実行後イベントリスナ の取得
	 * @return SQLUpdate実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<SqlUpdateEvent>> sqlUpdateListener() {
		return sqlUpdateListener;
	}

	/**
	 * SQLBatch実行後イベントリスナ の取得
	 * @return SQLBatch実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<SqlBatchEvent>> sqlBatchListener() {
		return sqlBatchListener;
	}

	/**
	 * Procedure実行後イベントリスナ の取得
	 * @return Procedure実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<ProcedureEvent>> procedureListener() {
		return procedureListener;
	}

	/**
	 * EntityQuery実行前イベントリスナ の取得
	 * @return EntityQuery実行前イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<BeforeEntityQueryEvent>> beforeEntityQueryListener() {
		return beforeEntityQueryListener;
	}

	/**
	 * EntityQuery実行後イベントリスナ の取得
	 * @return EntityQuery実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<AfterEntityQueryEvent>> afterEntityQueryListener() {
		return afterEntityQueryListener;
	}

	/**
	 * EntityInsert実行前イベントリスナ の取得
	 * @return EntityInsert実行前イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<BeforeEntityInsertEvent>> beforeEntityInsertListener() {
		return beforeEntityInsertListener;
	}

	/**
	 * EntityInsert実行後イベントリスナ の取得
	 * @return EntityInsert実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<AfterEntityInsertEvent>> afterEntityInsertListener() {
		return afterEntityInsertListener;
	}

	/**
	 * EntityUpdate実行前イベントリスナ の取得
	 * @return EntityUpdate実行前イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<BeforeEntityUpdateEvent>> beforeEntityUpdateListener() {
		return beforeEntityUpdateListener;
	}

	/**
	 * EntityUpdate実行後イベントリスナ の取得
	 * @return EntityUpdate実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<AfterEntityUpdateEvent>> afterEntityUpdateListener() {
		return afterEntityUpdateListener;
	}

	/**
	 * EntityDelete実行前イベントリスナ の取得
	 * @return EntityDelete実行前イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<BeforeEntityDeleteEvent>> beforeEntityDeleteListener() {
		return beforeEntityDeleteListener;
	}

	/**
	 * EntityDelete実行後イベントリスナ の取得
	 * @return EntityDelete実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<AfterEntityDeleteEvent>> afterEntityDeleteListener() {
		return afterEntityDeleteListener;
	}

	/**
	 * EntityBatchInsert実行前イベントリスナ の取得
	 * @return EntityBatchInsert実行前イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<BeforeEntityBatchInsertEvent>> beforeEntityBatchInsertListener() {
		return beforeEntityBatchInsertListener;
	}

	/**
	 * EntityBatchInsert実行後イベントリスナ の取得
	 * @return EntityBatchInsert実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<AfterEntityBatchInsertEvent>> afterEntityBatchInsertListener() {
		return afterEntityBatchInsertListener;
	}

	/**
	 * EntityBatchUpdate実行前イベントリスナ の取得
	 * @return EntityBatchUpdate実行前イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<BeforeEntityBatchUpdateEvent>> beforeEntityBatchUpdateListener() {
		return beforeEntityBatchUpdateListener;
	}

	/**
	 * EntityBatchUpdate実行後イベントリスナ の取得
	 * @return EntityBatchUpdate実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<AfterEntityBatchUpdateEvent>> afterEntityBatchUpdateListener() {
		return afterEntityBatchUpdateListener;
	}

	/**
	 * EntityBulkInsert実行前イベントリスナ の取得
	 * @return EntityBulkInsert実行前イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<BeforeEntityBulkInsertEvent>> beforeEntityBulkInsertListener() {
		return beforeEntityBulkInsertListener;
	}

	/**
	 * EntityBulkInsert実行後イベントリスナ の取得
	 * @return EntityBulkInsert実行後イベントリスナ
	 */
	protected final Optional<ExecutionConsumer<AfterEntityBulkInsertEvent>> afterEntityBulkInsertListener() {
		return afterEntityBulkInsertListener;
	}

	/**
	 * トランザクション開始後イベントリスナ の取得
	 * @return トランザクション開始後イベントリスナ
	 */
	protected final Optional<Consumer<AfterBeginTransactionEvent>> afterBeginTransactionListener() {
		return afterBeginTransactionListener;
	}

	/**
	 * トランザクション終了前イベントリスナ の取得
	 * @return トランザクション終了前イベントリスナ
	 */
	protected final Optional<Consumer<BeforeEndTransactionEvent>> beforeEndTransactionListener() {
		return beforeEndTransactionListener;
	}

	/**
	 * コミット前イベントリスナ の取得
	 * @return コミット前イベントリスナ
	 */
	protected final Optional<Consumer<BeforeCommitEvent>> beforeCommitListener() {
		return beforeCommitListener;
	}

	/**
	 * コミット後イベントリスナ の取得
	 * @return コミット後イベントリスナ
	 */
	protected final Optional<Consumer<AfterCommitEvent>> afterCommitListener() {
		return afterCommitListener;
	}

	/**
	 * ロールバック前イベントリスナ の取得
	 * @return ロールバック前イベントリスナ
	 */
	protected final Optional<Consumer<BeforeRollbackEvent>> beforeRollbackListener() {
		return beforeRollbackListener;
	}

	/**
	 * ロールバック後イベントリスナ の取得
	 * @return ロールバック後イベントリスナ
	 */
	protected final Optional<Consumer<AfterRollbackEvent>> afterRollbackListener() {
		return afterRollbackListener;
	}

	/**
	 * ExecutionContext初期化後イベントリスナ の設定
	 * @param ExecutionContext初期化後イベントリスナ
	 */
	protected final void afterInitializeExecutionContextListener(
			final Consumer<AfterInitializeExecutionContextEvent> listener) {
		this.afterInitializeExecutionContextListener = Optional.ofNullable(listener);
	}

	/**
	 * パラメータ設定前イベントリスナ の設定
	 * @param パラメータ設定前イベントリスナ
	 */
	protected final void beforeSetParameterListener(final Consumer<BeforeSetParameterEvent> listener) {
		this.beforeSetParameterListener = Optional.ofNullable(listener);
	}

	/**
	 * SQL変換イベントリスナ の設定
	 * @param SQL変換イベントリスナ
	 */
	protected final void transformSqlListener(final Consumer<TransformSqlEvent> listener) {
		this.transformSqlListener = Optional.ofNullable(listener);
	}

	/**
	 * SQLパース前イベントリスナ の設定
	 * @param SQLパース前イベントリスナ
	 */
	protected final void beforeParseSqlListener(final Consumer<BeforeParseSqlEvent> listener) {
		this.beforeParseSqlListener = Optional.ofNullable(listener);
	}

	/**
	 * 出力パラメータ取得後イベントリスナ の設定
	 * @param 出力パラメータ取得後イベントリスナ
	 */
	protected final void afterGetOutParameterListener(final Consumer<AfterGetOutParameterEvent> listener) {
		this.afterGetOutParameterListener = Optional.ofNullable(listener);
	}

	/**
	 * PreparedStatement生成後イベントリスナ の設定
	 * @param PreparedStatement生成後イベントリスナ
	 */
	protected final void afterCreatePreparedStatementListener(
			final ExecutionConsumer<AfterCreatePreparedStatementEvent> listener) {
		this.afterCreatePreparedStatementListener = Optional.ofNullable(listener);
	}

	/**
	 * CallableStatement生成後イベントリスナ の設定
	 * @param CallableStatement生成後イベントリスナ
	 */
	protected final void afterCreateCallableStatementListener(
			final ExecutionConsumer<AfterCreateCallableStatementEvent> listener) {
		this.afterCreateCallableStatementListener = Optional.ofNullable(listener);
	}

	/**
	 * SQLQuery実行後イベントリスナ の設定
	 * @param SQLQuery実行後イベントリスナ
	 */
	protected final void sqlQueryListener(final ExecutionConsumer<SqlQueryEvent> listener) {
		this.sqlQueryListener = Optional.ofNullable(listener);
	}

	/**
	 * SQLUpdate実行後イベントリスナ の設定
	 * @param SQLUpdate実行後イベントリスナ
	 */
	protected final void sqlUpdateListener(final ExecutionConsumer<SqlUpdateEvent> listener) {
		this.sqlUpdateListener = Optional.ofNullable(listener);
	}

	/**
	 * SQLBatch実行後イベントリスナ の設定
	 * @param SQLBatch実行後イベントリスナ
	 */
	protected final void sqlBatchListener(final ExecutionConsumer<SqlBatchEvent> listener) {
		this.sqlBatchListener = Optional.ofNullable(listener);
	}

	/**
	 * Procedure実行後イベントリスナ の設定
	 * @param Procedure実行後イベントリスナ
	 */
	protected final void procedureListener(final ExecutionConsumer<ProcedureEvent> listener) {
		this.procedureListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityQuery実行前イベントリスナ の設定
	 * @param EntityQuery実行前イベントリスナ
	 */
	protected final void beforeEntityQueryListener(final ExecutionConsumer<BeforeEntityQueryEvent> listener) {
		this.beforeEntityQueryListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityQuery実行後イベントリスナ の設定
	 * @param EntityQuery実行後イベントリスナ
	 */
	protected final void afterEntityQueryListener(final ExecutionConsumer<AfterEntityQueryEvent> listener) {
		this.afterEntityQueryListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityInsert実行前イベントリスナ の設定
	 * @param EntityInsert実行前イベントリスナ
	 */
	protected final void beforeEntityInsertListener(final ExecutionConsumer<BeforeEntityInsertEvent> listener) {
		this.beforeEntityInsertListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityInsert実行後イベントリスナ の設定
	 * @param EntityInsert実行後イベントリスナ
	 */
	protected final void afterEntityInsertListener(final ExecutionConsumer<AfterEntityInsertEvent> listener) {
		this.afterEntityInsertListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityUpdate実行前イベントリスナ の設定
	 * @param EntityUpdate実行前イベントリスナ
	 */
	protected final void beforeEntityUpdateListener(final ExecutionConsumer<BeforeEntityUpdateEvent> listener) {
		this.beforeEntityUpdateListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityUpdate実行後イベントリスナ の設定
	 * @param EntityUpdate実行後イベントリスナ
	 */
	protected final void afterEntityUpdateListener(final ExecutionConsumer<AfterEntityUpdateEvent> listener) {
		this.afterEntityUpdateListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityDelete実行前イベントリスナ の設定
	 * @param EntityDelete実行前イベントリスナ
	 */
	protected final void beforeEntityDeleteListener(final ExecutionConsumer<BeforeEntityDeleteEvent> listener) {
		this.beforeEntityDeleteListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityDelete実行後イベントリスナ の設定
	 * @param EntityDelete実行後イベントリスナ
	 */
	protected final void afterEntityDeleteListener(final ExecutionConsumer<AfterEntityDeleteEvent> listener) {
		this.afterEntityDeleteListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityBatchInsert実行前イベントリスナ の設定
	 * @param EntityBatchInsert実行前イベントリスナ
	 */
	protected final void beforeEntityBatchInsertListener(
			final ExecutionConsumer<BeforeEntityBatchInsertEvent> listener) {
		this.beforeEntityBatchInsertListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityBatchInsert実行後イベントリスナ の設定
	 * @param EntityBatchInsert実行後イベントリスナ
	 */
	protected final void afterEntityBatchInsertListener(final ExecutionConsumer<AfterEntityBatchInsertEvent> listener) {
		this.afterEntityBatchInsertListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityBatchUpdate実行前イベントリスナ の設定
	 * @param EntityBatchUpdate実行前イベントリスナ
	 */
	protected final void beforeEntityBatchUpdateListener(
			final ExecutionConsumer<BeforeEntityBatchUpdateEvent> listener) {
		this.beforeEntityBatchUpdateListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityBatchUpdate実行後イベントリスナ の設定
	 * @param EntityBatchUpdate実行後イベントリスナ
	 */
	protected final void afterEntityBatchUpdateListener(final ExecutionConsumer<AfterEntityBatchUpdateEvent> listener) {
		this.afterEntityBatchUpdateListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityBulkInsert実行前イベントリスナ の設定
	 * @param EntityBulkInsert実行前イベントリスナ
	 */
	protected final void beforeEntityBulkInsertListener(final ExecutionConsumer<BeforeEntityBulkInsertEvent> listener) {
		this.beforeEntityBulkInsertListener = Optional.ofNullable(listener);
	}

	/**
	 * EntityBulkInsert実行後イベントリスナ の設定
	 * @param EntityBulkInsert実行後イベントリスナ
	 */
	protected final void afterEntityBulkInsertListener(final ExecutionConsumer<AfterEntityBulkInsertEvent> listener) {
		this.afterEntityBulkInsertListener = Optional.ofNullable(listener);
	}

	/**
	 * トランザクション開始後イベントリスナ の設定
	 * @param トランザクション開始後イベントリスナ
	 */
	protected final void afterBeginTransactionListener(final Consumer<AfterBeginTransactionEvent> listener) {
		this.afterBeginTransactionListener = Optional.ofNullable(listener);
	}

	/**
	 * トランザクション終了前イベントリスナ の設定
	 * @param トランザクション終了前イベントリスナ
	 */
	protected final void beforeEndTransactionListener(final Consumer<BeforeEndTransactionEvent> listener) {
		this.beforeEndTransactionListener = Optional.ofNullable(listener);
	}

	/**
	 * コミット前イベントリスナ の設定
	 * @param コミット前イベントリスナ
	 */
	protected final void beforeCommitListener(final Consumer<BeforeCommitEvent> listener) {
		this.beforeCommitListener = Optional.ofNullable(listener);
	}

	/**
	 * コミット後イベントリスナ の設定
	 * @param コミット後イベントリスナ
	 */
	protected final void afterCommitListener(final Consumer<AfterCommitEvent> listener) {
		this.afterCommitListener = Optional.ofNullable(listener);
	}

	/**
	 * ロールバック前イベントリスナ の設定
	 * @param ロールバック前イベントリスナ
	 */
	protected final void beforeRollbackListener(final Consumer<BeforeRollbackEvent> listener) {
		this.beforeRollbackListener = Optional.ofNullable(listener);
	}

	/**
	 * ロールバック後イベントリスナ の設定
	 * @param ロールバック後イベントリスナ
	 */
	protected final void afterRollbackListener(final Consumer<AfterRollbackEvent> listener) {
		this.afterRollbackListener = Optional.ofNullable(listener);
	}

}
