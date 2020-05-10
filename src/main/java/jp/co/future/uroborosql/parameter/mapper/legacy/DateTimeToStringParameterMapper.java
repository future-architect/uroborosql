/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper.legacy;

import java.sql.Connection;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalAccessor;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

/**
 * {@link java.time.LocalDateTime} / {@link java.time.OffsetDateTime} / {@link java.time.ZonedDateTime} を文字列に変換する{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class DateTimeToStringParameterMapper implements BindParameterMapper<TemporalAccessor> {
	/**
	 * LocalDateTime/OffsetDateTime/ZonedDateTime から yyyyMMddHHmmssSSS文字列 に変換するためのフォーマッター.<br>
	 * Java8での不具合のため、DateTimeFormatterはBuilderを使用して生成する.
	 */
	private static final DateTimeFormatter FORMATTER = new DateTimeFormatterBuilder()
			.appendPattern("yyyyMMddHHmmss").appendValue(ChronoField.MILLI_OF_SECOND, 3).toFormatter();

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
		return LocalDateTime.class.isInstance(object) ||
				OffsetDateTime.class.isInstance(object) ||
				ZonedDateTime.class.isInstance(object);
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
