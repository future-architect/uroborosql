/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;
import java.util.Arrays;

/**
 * {@link int}配列用{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class IntArrayParameterMapper implements BindParameterMapper<int[]> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<int[]> targetType() {
		return int[].class;
	}

	@Override
	public Object toJdbc(final int[] original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return JdbcParameterFactory.createArrayOf(connection, "INTEGER",
				Arrays.stream(original).mapToObj(Integer::valueOf).toArray());
	}
}
