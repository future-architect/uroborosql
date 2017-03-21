package jp.co.future.uroborosql.filter;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * SqlFilterの抽象親クラス
 * 子クラスはこのクラスを継承して、必要に応じてメソッドをオーバーライドする
 *
 * @author H.Sugimoto
 */
public abstract class AbstractSqlFilter implements SqlFilter {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#initialize()
	 */
	@Override
	public void initialize() {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doParameter(jp.co.future.uroborosql.parameter.Parameter)
	 */
	@Override
	public Parameter doParameter(final Parameter parameter) {
		return parameter;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doOutParameter(java.lang.String, java.lang.Object)
	 */
	@Override
	public Object doOutParameter(final String key, final Object val) {
		return val;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doTransformSql(jp.co.future.uroborosql.context.SqlContext, java.lang.String)
	 */
	@Override
	public String doTransformSql(final SqlContext sqlContext, final String sql) {
		return sql;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doPreparedStatement(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement)
	 */
	@Override
	public PreparedStatement doPreparedStatement(final SqlContext sqlContext, final PreparedStatement preparedStatement)
			throws SQLException {
		return preparedStatement;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doCallableStatement(jp.co.future.uroborosql.context.SqlContext, java.sql.CallableStatement)
	 */
	@Override
	public CallableStatement doCallableStatement(final SqlContext sqlContext, final CallableStatement callableStatement)
			throws SQLException {
		return callableStatement;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doQuery(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, java.sql.ResultSet)
	 */
	@Override
	public ResultSet doQuery(final SqlContext sqlContext, final PreparedStatement preparedStatement,
			final ResultSet resultSet) {
		return resultSet;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doUpdate(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, int)
	 */
	@Override
	public int doUpdate(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int result) {
		return result;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doBatch(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, int[])
	 */
	@Override
	public int[] doBatch(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int[] result) {
		return result;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.SqlFilter#doProcedure(jp.co.future.uroborosql.context.SqlContext, java.sql.CallableStatement, boolean)
	 */
	@Override
	public boolean doProcedure(final SqlContext sqlContext, final CallableStatement callableStatement,
			final boolean result) {
		return result;
	}

}
