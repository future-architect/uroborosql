/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.SQLException;
import java.sql.SQLType;
import java.util.Map;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.fluent.Procedure;

/**
 * Procedure実装
 *
 * @author H.Sugimoto
 */
final class ProcedureImpl extends AbstractSqlFluent<Procedure> implements Procedure {
	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 */
	ProcedureImpl(final SqlAgent agent, final ExecutionContext context) {
		super(agent, context);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.Procedure#call()
	 */
	@Override
	public Map<String, Object> call() throws SQLException {
		return agent().procedure(context());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ProcedureFluent#outParam(java.lang.String, int)
	 */
	@Override
	public Procedure outParam(final String paramName, final int sqlType) {
		context().outParam(paramName, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ProcedureFluent#outParam(java.lang.String, java.sql.SQLType)
	 */
	@Override
	public Procedure outParam(final String paramName, final SQLType sqlType) {
		context().outParam(paramName, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ProcedureFluent#inOutParam(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public <V> Procedure inOutParam(final String paramName, final V value, final int sqlType) {
		context().inOutParam(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ProcedureFluent#inOutParamIfAbsent(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public <V> Procedure inOutParamIfAbsent(final String paramName, final V value, final int sqlType) {
		context().inOutParamIfAbsent(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ProcedureFluent#inOutParam(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public <V> Procedure inOutParam(final String paramName, final V value, final SQLType sqlType) {
		context().inOutParam(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ProcedureFluent#inOutParamIfAbsent(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public <V> Procedure inOutParamIfAbsent(final String paramName, final V value, final SQLType sqlType) {
		context().inOutParamIfAbsent(paramName, value, sqlType);
		return this;
	}

}
