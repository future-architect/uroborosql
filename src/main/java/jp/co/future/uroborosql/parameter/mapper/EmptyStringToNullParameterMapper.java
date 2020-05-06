/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;

/**
 * 空文字をNULLに変換する{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class EmptyStringToNullParameterMapper implements BindParameterMapper<String> {
	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<String> targetType() {
		return String.class;
	}

	@Override
	public boolean canAccept(final Object object) {
		if (object instanceof String) {
			return ((String) object).isEmpty();
		} else {
			return false;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final String original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return original.isEmpty() ? null : original;
	}
}
