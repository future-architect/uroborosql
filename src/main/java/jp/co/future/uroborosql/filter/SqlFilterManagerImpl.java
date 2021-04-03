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

import jp.co.future.uroborosql.context.SqlContext;
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
		Parameter param = parameter;
		for (final SqlFilter filter : getFilters()) {
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
		Object obj = val;
		for (final SqlFilter filter : getFilters()) {
			obj = filter.doOutParameter(key, obj);
		}
		return obj;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doTransformSql(jp.co.future.uroborosql.context.SqlContext, java.lang.String)
	 */
	@Override
	public String doTransformSql(final SqlContext sqlContext, final String sql) {
		if (getFilters().isEmpty()) {
			return sql;
		}
		String newSql = sql;
		for (final SqlFilter filter : getFilters()) {
			newSql = filter.doTransformSql(sqlContext, newSql);
		}
		return newSql;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doPreparedStatement(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement)
	 */
	@Override
	public PreparedStatement doPreparedStatement(final SqlContext sqlContext, final PreparedStatement preparedStatement)
			throws SQLException {
		if (getFilters().isEmpty()) {
			return preparedStatement;
		}
		PreparedStatement ps = preparedStatement;
		for (final SqlFilter filter : getFilters()) {
			ps = filter.doPreparedStatement(sqlContext, ps);
		}
		return ps;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doCallableStatement(jp.co.future.uroborosql.context.SqlContext, java.sql.CallableStatement)
	 */
	@Override
	public CallableStatement doCallableStatement(final SqlContext sqlContext, final CallableStatement callableStatement)
			throws SQLException {
		if (getFilters().isEmpty()) {
			return callableStatement;
		}
		CallableStatement cs = callableStatement;
		for (final SqlFilter filter : getFilters()) {
			cs = filter.doCallableStatement(sqlContext, cs);
		}
		return cs;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doQuery(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, java.sql.ResultSet)
	 */
	@Override
	public ResultSet doQuery(final SqlContext sqlContext, final PreparedStatement preparedStatement,
			final ResultSet resultSet) throws SQLException {
		if (getFilters().isEmpty()) {
			return resultSet;
		}
		ResultSet rs = resultSet;
		for (final SqlFilter filter : getFilters()) {
			rs = filter.doQuery(sqlContext, preparedStatement, rs);
		}
		return rs;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doUpdate(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, int)
	 */
	@Override
	public int doUpdate(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int result)
			throws SQLException {
		if (getFilters().isEmpty()) {
			return result;
		}
		int rs = result;
		for (final SqlFilter filter : getFilters()) {
			rs = filter.doUpdate(sqlContext, preparedStatement, rs);
		}
		return rs;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doBatch(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, int[])
	 */
	@Override
	public int[] doBatch(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int[] result)
			throws SQLException {
		if (getFilters().isEmpty()) {
			return result;
		}
		int[] rs = result;
		for (final SqlFilter filter : getFilters()) {
			rs = filter.doBatch(sqlContext, preparedStatement, rs);
		}
		return rs;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doProcedure(jp.co.future.uroborosql.context.SqlContext, java.sql.CallableStatement, boolean)
	 */
	@Override
	public boolean doProcedure(final SqlContext sqlContext, final CallableStatement callableStatement,
			final boolean result) throws SQLException {
		if (getFilters().isEmpty()) {
			return result;
		}
		boolean rs = result;
		for (final SqlFilter filter : getFilters()) {
			rs = filter.doProcedure(sqlContext, callableStatement, rs);
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
