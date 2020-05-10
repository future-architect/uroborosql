/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper.legacy;

import java.sql.Connection;
import java.sql.Time;
import java.time.format.DateTimeFormatter;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

/**
 * {@link java.sql.Time}を文字列に変換する{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class SqlTimeToStringParameterMapper implements BindParameterMapper<Time> {

	private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("HHmmss");

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<Time> targetType() {
		return Time.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final Time original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return FORMATTER.format(original.toLocalTime());
	}
}
