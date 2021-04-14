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
 * イベントサブスクライバ.
 *
 * @author yanagihara
 */
public interface EventSubscriber {
	/**
	 * 初期化メソッド.
	 */
	default void initialize() {
		// noop
	}

	/**
	 * Parameterに対する編集処理を行う.
	 *
	 * @param event パラメータ編集イベント
	 *
	 * @return 変換後パラメータ
	 */
	default Parameter doParameter(final ParameterEvent event) {
		return event.getParameter();
	}

	/**
	 * ストアドプロシージャのOutParameterに対する編集処理を行う.
	 *
	 * @param event 出力パラメータ編集イベント
	 *
	 * @return 変換後の値
	 */
	default Object doOutParameter(final OutParameterEvent event) {
		return event.getValue();
	}

	/**
	 * PreparedStatementに対する編集処理を行う.
	 *
	 * @param event PreparedStatement作成イベント
	 *
	 * @return 編集後のPreparedStatement
	 * @throws SQLException SQL例外
	 */
	default PreparedStatement doPreparedStatement(final PreparedStatementEvent event) throws SQLException {
		return event.getPreparedStatement();
	}

	/**
	 * CallableStatementに対する編集処理を行う.
	 *
	 * @param event CallableStatement作成イベント
	 *
	 * @return 編集後のCallableStatement
	 * @throws SQLException SQL例外
	 */
	default CallableStatement doCallableStatement(final CallableStatementEvent event) throws SQLException {
		return event.getCallableStatement();
	}

	/**
	 * SQLに対する変換処理を行う.
	 *
	 * @param event SQL変換イベント
	 *
	 * @return 変換後のSQL文字列
	 */
	default String doTransformSql(final TransformSqlEvent event) {
		return event.getOriginalSql();
	}

	/**
	 * パラメータに対する編集処理を行う[INSERT].
	 *
	 * @param event パラメータ編集イベント
	 */
	default void doInsertParams(final InsertParameterEvent event) {
		// noop
	}

	/**
	 * パラメータに対する編集処理を行う[UPDATE].
	 *
	 * @param event パラメータ編集イベント
	 */
	default void doUpdateParams(final UpdateParameterEvent event) {
		// noop
	}

	/**
	 * パラメータに対する編集処理を行う[DELETE].
	 *
	 * @param event パラメータ編集イベント
	 */
	default void doDeleteParams(final DeleteParameterEvent event) {
		// noop
	}

	/**
	 * パラメータに対する編集処理を行う[BULK-INSERT].
	 *
	 * @param event パラメータ編集イベント
	 */
	default void doBulkInsertParams(final BulkInsertParameterEvent event) {
		// noop
	}

	/**
	 * 検索処理に対する編集処理を行う.
	 *
	 * @param event クエリ実行結果イベント
	 *
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	default ResultSet doQuery(final QueryResultEvent event) throws SQLException {
		return event.getResultSet();
	}

	/**
	 * 更新処理結果に対する編集処理を行う.
	 *
	 * @param event 更新実行結果イベント
	 *
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	default int doUpdate(final UpdateResultEvent event) throws SQLException {
		return event.getResult();
	}

	/**
	 * バッチ処理結果に対する編集処理を行う.
	 *
	 * @param event バッチ実行結果イベント
	 *
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	default int[] doBatch(final BatchResultEvent event) throws SQLException {
		return event.getResult();
	}

	/**
	 * CallableProcedure処理結果に対する編集処理を行う.
	 *
	 * @param event プロシージャ実行結果イベント
	 *
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	default boolean doProcedure(final ProcedureResultEvent event) throws SQLException {
		return event.getResult();
	}

	/**
	 * トランザクション開始前処理を行う.
	 *
	 * @param event トランザクション開始前イベント
	 */
	default void doBeforeTransaction(final BeforeTransactionEvent event) {
		// noop
	}

	/**
	 * トランザクション終了後処理を行う.
	 *
	 * @param event トランザクション終了後イベント
	 */
	default void doAfterTransaction(final AfterTransactionEvent event) {
		// noop
	}

	/**
	 * コミット実行前処理を行う.
	 *
	 * @param event コミット実行前イベント
	 */
	default void doBeforeCommit(final BeforeCommitEvent event) {
		// noop
	}

	/**
	 * コミット実行後処理を行う.
	 *
	 * @param event コミット実行後イベント
	 */
	default void doAfterCommit(final AfterCommitEvent event) {
		// noop
	}

	/**
	 * ロールバック実行前処理を行う.
	 *
	 * @param event ロールバック実行前イベント
	 */
	default void doBeforeRollback(final BeforeRollbackEvent event) {
		// noop
	}

	/**
	 * ロールバック実行後処理を行う.
	 *
	 * @param event ロールバック実行後イベント
	 */
	default void doAfterRollback(final AfterRollbackEvent event) {
		// noop
	}
}
