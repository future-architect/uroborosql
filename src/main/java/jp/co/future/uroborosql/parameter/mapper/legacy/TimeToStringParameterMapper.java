/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper.legacy;

import java.sql.Connection;
import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

/**
 * {@link java.time.LocalTime} / {@link java.time.OffsetTime} を文字列に変換する{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class TimeToStringParameterMapper implements BindParameterMapper<TemporalAccessor> {
	private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("HHmmss");

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<TemporalAccessor> targetType() {
		return TemporalAccessor.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#canAccept(java.lang.Object)
	 */
	@Override
	public boolean canAccept(final Object object) {
		return object instanceof LocalTime ||
				object instanceof OffsetTime;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final TemporalAccessor original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return FORMATTER.format(original);
	}
}
