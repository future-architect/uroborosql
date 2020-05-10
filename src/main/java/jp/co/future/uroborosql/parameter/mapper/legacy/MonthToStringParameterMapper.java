/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper.legacy;

import java.sql.Connection;
import java.time.Month;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * {@link java.time.Month}を文字列に変換する{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class MonthToStringParameterMapper implements BindParameterMapper<Month> {
	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<Month> targetType() {
		return Month.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final Month original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return StringUtils.leftPad(String.valueOf(original.getValue()), 2, '0');
	}
}
