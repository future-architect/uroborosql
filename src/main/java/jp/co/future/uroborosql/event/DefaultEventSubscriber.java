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
 * {@link EventSubscriber}のデフォルト実装.
 *
 * @author yanagihara
 */
public class DefaultEventSubscriber implements EventSubscriber {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#initialize()
	 */
	@Override
	public void initialize() {
		// noop
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doParameter(ParameterEvent)
	 */
	@Override
	public Parameter doParameter(final ParameterEvent event) {
		return event.getParameter();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doOutParameter(OutParameterEvent)
	 */
	@Override
	public Object doOutParameter(final OutParameterEvent event) {
		return event.getValue();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doPreparedStatement(PreparedStatementEvent)
	 */
	@Override
	public PreparedStatement doPreparedStatement(final PreparedStatementEvent event) {
		return event.getPreparedStatement();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doCallableStatement(CallableStatementEvent)
	 */
	@Override
	public CallableStatement doCallableStatement(final CallableStatementEvent event) {
		return event.getCallableStatement();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doTransformSql(TransformSqlEvent)
	 */
	@Override
	public String doTransformSql(final TransformSqlEvent event) {
		return event.getOriginalSql();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doInsertParams(InsertParameterEvent)
	 */
	@Override
	public void doInsertParams(final InsertParameterEvent event) {
		// noop
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doUpdateParams(UpdateParameterEvent)
	 */
	@Override
	public void doUpdateParams(final UpdateParameterEvent event) {
		// noop
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doDeleteParams(DeleteParameterEvent)
	 */
	@Override
	public void doDeleteParams(final DeleteParameterEvent event) {
		// noop
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doBulkInsertParams(BulkInsertParameterEvent)
	 */
	@Override
	public void doBulkInsertParams(final BulkInsertParameterEvent event) {
		// noop
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doQuery(QueryResultEvent)
	 */
	@Override
	public ResultSet doQuery(final QueryResultEvent event) throws SQLException {
		return event.getResultSet();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doUpdate(UpdateResultEvent)
	 */
	@Override
	public int doUpdate(final UpdateResultEvent event) throws SQLException {
		return event.getResult();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doBatch(BatchResultEvent)
	 */
	@Override
	public int[] doBatch(final BatchResultEvent event) throws SQLException {
		return event.getResult();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doProcedure(ProcedureResultEvent)
	 */
	@Override
	public boolean doProcedure(final ProcedureResultEvent event) throws SQLException {
		return event.getResult();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doBeforeTransaction(BeforeTransactionEvent)
	 */
	@Override
	public void doBeforeTransaction(final BeforeTransactionEvent event) {
		// noop
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doAfterTransaction(AfterTransactionEvent)
	 */
	@Override
	public void doAfterTransaction(final AfterTransactionEvent event) {
		// noop
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doBeforeCommit(BeforeCommitEvent)
	 */
	@Override
	public void doBeforeCommit(final BeforeCommitEvent event) {
		// noop
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doAfterCommit(AfterCommitEvent)
	 */
	@Override
	public void doAfterCommit(final AfterCommitEvent event) {
		// noop
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doBeforeRollback(BeforeRollbackEvent)
	 */
	@Override
	public void doBeforeRollback(final BeforeRollbackEvent event) {
		// noop
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doAfterRollback(AfterRollbackEvent)
	 */
	@Override
	public void doAfterRollback(final AfterRollbackEvent event) {
		// noop
	}
}
