/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper.legacy;

import java.sql.Connection;
import java.time.MonthDay;
import java.time.format.DateTimeFormatter;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

/**
 * {@link java.time.MonthDay}を文字列に変換する{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class MonthDayToStringParameterMapper implements BindParameterMapper<MonthDay> {

	private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("MMdd");

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<MonthDay> targetType() {
		return MonthDay.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final MonthDay original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return original.format(FORMATTER);
	}
}
