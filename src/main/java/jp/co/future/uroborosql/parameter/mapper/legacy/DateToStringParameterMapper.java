/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper.legacy;

import java.sql.Connection;
import java.sql.Time;
import java.sql.Timestamp;
import java.time.Clock;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperWithClock;

/**
 * {@link java.util.Date}を文字列に変換する{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class DateToStringParameterMapper implements BindParameterMapperWithClock<Date> {

	private Clock clock;

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<Date> targetType() {
		return Date.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#canAccept(java.lang.Object)
	 */
	@Override
	public boolean canAccept(final Object object) {
		return object instanceof Date && !(object instanceof Time)
				&& !(object instanceof Timestamp);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final Date original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		if (original instanceof java.sql.Date) {
			return DateTimeFormatter.BASIC_ISO_DATE.format(((java.sql.Date) original).toLocalDate());
		} else {
			return DateTimeFormatter.BASIC_ISO_DATE.format(original.toInstant().atZone(clock.getZone()).toLocalDate());
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapperWithClock#getClock()
	 */
	@Override
	public Clock getClock() {
		return clock;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapperWithClock#setClock(java.time.Clock)
	 */
	@Override
	public void setClock(final Clock clock) {
		this.clock = clock;
	}
}
