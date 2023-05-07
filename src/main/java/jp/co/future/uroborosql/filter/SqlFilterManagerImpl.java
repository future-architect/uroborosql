/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.filter;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * SQLフィルター管理クラス実装
 *
 * 登録されたSQLフィルターを順に実行する。実行順は登録された順となる。
 *
 * @author H.Sugimoto
 */
public class SqlFilterManagerImpl implements SqlFilterManager {
	/** SqlFilterのリスト */
	private List<SqlFilter> filters = new ArrayList<>();

	/**
	 * コンストラクタ
	 */
	public SqlFilterManagerImpl() {
		// do nothing
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#initialize()
	 */
	@Override
	public void initialize() {
		filters.forEach(SqlFilter::initialize);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doParameter(jp.co.future.uroborosql.parameter.Parameter)
	 */
	@Override
	public Parameter doParameter(final Parameter parameter) {
		if (getFilters().isEmpty()) {
			return parameter;
		}
		var param = parameter;
		for (var filter : getFilters()) {
			param = filter.doParameter(param);
		}
		return param;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doOutParameter(java.lang.String, java.lang.Object)
	 */
	@Override
	public Object doOutParameter(final String key, final Object val) {
		if (getFilters().isEmpty()) {
			return val;
		}
		var obj = val;
		for (var filter : getFilters()) {
			obj = filter.doOutParameter(key, obj);
		}
		return obj;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doTransformSql(jp.co.future.uroborosql.context.ExecutionContext, java.lang.String)
	 */
	@Override
	public String doTransformSql(final ExecutionContext executionContext, final String sql) {
		if (getFilters().isEmpty()) {
			return sql;
		}
		var newSql = sql;
		for (var filter : getFilters()) {
			newSql = filter.doTransformSql(executionContext, newSql);
		}
		return newSql;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doPreparedStatement(jp.co.future.uroborosql.context.ExecutionContext, java.sql.PreparedStatement)
	 */
	@Override
	public PreparedStatement doPreparedStatement(final ExecutionContext executionContext,
			final PreparedStatement preparedStatement)
			throws SQLException {
		if (getFilters().isEmpty()) {
			return preparedStatement;
		}
		var ps = preparedStatement;
		for (var filter : getFilters()) {
			ps = filter.doPreparedStatement(executionContext, ps);
		}
		return ps;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doCallableStatement(jp.co.future.uroborosql.context.ExecutionContext, java.sql.CallableStatement)
	 */
	@Override
	public CallableStatement doCallableStatement(final ExecutionContext executionContext,
			final CallableStatement callableStatement)
			throws SQLException {
		if (getFilters().isEmpty()) {
			return callableStatement;
		}
		var cs = callableStatement;
		for (var filter : getFilters()) {
			cs = filter.doCallableStatement(executionContext, cs);
		}
		return cs;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doQuery(jp.co.future.uroborosql.context.ExecutionContext, java.sql.PreparedStatement, java.sql.ResultSet)
	 */
	@Override
	public ResultSet doQuery(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
			final ResultSet resultSet) throws SQLException {
		if (getFilters().isEmpty()) {
			return resultSet;
		}
		var rs = resultSet;
		for (var filter : getFilters()) {
			rs = filter.doQuery(executionContext, preparedStatement, rs);
		}
		return rs;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doUpdate(jp.co.future.uroborosql.context.ExecutionContext, java.sql.PreparedStatement, int)
	 */
	@Override
	public int doUpdate(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
			final int result)
			throws SQLException {
		if (getFilters().isEmpty()) {
			return result;
		}
		var rs = result;
		for (var filter : getFilters()) {
			rs = filter.doUpdate(executionContext, preparedStatement, rs);
		}
		return rs;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doBatch(jp.co.future.uroborosql.context.ExecutionContext, java.sql.PreparedStatement, int[])
	 */
	@Override
	public int[] doBatch(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
			final int[] result)
			throws SQLException {
		if (getFilters().isEmpty()) {
			return result;
		}
		var rs = result;
		for (var filter : getFilters()) {
			rs = filter.doBatch(executionContext, preparedStatement, rs);
		}
		return rs;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doProcedure(jp.co.future.uroborosql.context.ExecutionContext, java.sql.CallableStatement, boolean)
	 */
	@Override
	public boolean doProcedure(final ExecutionContext executionContext, final CallableStatement callableStatement,
			final boolean result) throws SQLException {
		if (getFilters().isEmpty()) {
			return result;
		}
		var rs = result;
		for (var filter : getFilters()) {
			rs = filter.doProcedure(executionContext, callableStatement, rs);
		}
		return rs;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilterManager#getFilters()
	 */
	@Override
	public List<SqlFilter> getFilters() {
		return filters;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilterManager#setFilters(java.util.List)
	 */
	@Override
	public void setFilters(final List<SqlFilter> filters) {
		if (filters == null) {
			throw new IllegalArgumentException("filters is null.");
		}
		this.filters = filters;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilterManager#addSqlFilter(jp.co.future.uroborosql.filter.SqlFilter)
	 */
	@Override
	public SqlFilterManager addSqlFilter(final SqlFilter filter) {
		if (filter == null) {
			throw new IllegalArgumentException("filter is null.");
		}
		filters.add(filter);
		return this;
	}

}
