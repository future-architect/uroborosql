package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;
import java.util.Date;

/**
 * {@link java.util.Date}用{@link BindParameterMapper}
 *
 * @author ota
 */
public class DateParameterMapper implements BindParameterMapper<Date> {

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
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final Date original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		if (original instanceof java.sql.Date || original instanceof java.sql.Timestamp
				|| original instanceof java.sql.Time) {
			return original;
		}
		return new java.sql.Timestamp(original.getTime());
	}
}
