/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;
import java.util.Optional;

/**
 * {@link Optional}用{@link BindParameterMapper}
 *
 * @author ota
 */
public class OptionalParameterMapper implements BindParameterMapper<Optional<?>> {
	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public Class<Optional<?>> targetType() {
		return (Class) Optional.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final Optional<?> original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return original.isPresent() ? parameterMapperManager.toJdbc(original.get(), connection) : null;
	}
}
