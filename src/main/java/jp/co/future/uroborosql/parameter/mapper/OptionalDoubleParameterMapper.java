/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;
import java.util.OptionalDouble;

/**
 * {@link OptionalDouble}用{@link BindParameterMapper}
 *
 * @author ota
 */
public class OptionalDoubleParameterMapper implements BindParameterMapper<OptionalDouble> {
	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<OptionalDouble> targetType() {
		return OptionalDouble.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final OptionalDouble original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return original.isPresent() ? original.getAsDouble() : null;
	}
}
