/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;

/**
 * {@link Long}配列用{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class LongWrapperArrayParameterMapper implements BindParameterMapper<Long[]> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<Long[]> targetType() {
		return Long[].class;
	}

	@Override
	public Object toJdbc(final Long[] original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return JdbcParameterFactory.createArrayOf(connection, "BIGINT", original);
	}
}
