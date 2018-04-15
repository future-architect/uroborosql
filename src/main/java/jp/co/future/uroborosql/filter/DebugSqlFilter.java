/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.filter;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.parameter.Parameter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * デバッグログを出力するSqlFilter
 *
 * @author H.Sugimoto
 */
public class DebugSqlFilter extends AbstractSqlFilter {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(DebugSqlFilter.class);

	@Override
	public Parameter doParameter(final Parameter parameter) {
		LOG.debug("Parameter:{}", parameter);
		return parameter;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doOutParameter(java.lang.String, java.lang.Object)
	 */
	@Override
	public Object doOutParameter(final String key, final Object val) {
		LOG.debug("Out parameter - Key:{}, value:{}", key, val);
		return val;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doQuery(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, java.sql.ResultSet)
	 */
	@Override
	public ResultSet doQuery(final SqlContext sqlContext, final PreparedStatement preparedStatement,
			final ResultSet resultSet) {
		LOG.debug("SQL:{} executed.", sqlContext.getSqlName());
		return resultSet;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doUpdate(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, int)
	 */
	@Override
	public int doUpdate(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int result) {
		LOG.debug("SQL:{} executed. Count:{} items.", sqlContext.getSqlName(), result);
		return result;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doBatch(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, int[])
	 */
	@Override
	public int[] doBatch(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int[] result) {
		if (LOG.isDebugEnabled()) {
			int[] counts = result;
			try {
				counts = new int[] { preparedStatement.getUpdateCount() };
			} catch (SQLException ex) {
				ex.printStackTrace();
			}

			StringBuilder builder = new StringBuilder();
			for (int val : counts) {
				builder.append(val).append(", ");
			}
			LOG.debug("SQL:{} executed. Result:{}", sqlContext.getSqlName(), builder.toString());
		}
		return result;
	}
}