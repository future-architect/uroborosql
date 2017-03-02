package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;

/**
 * {@link String}配列用{@link BindParameterMapper}
 *
 * @author ota
 */
public class StringArrayParameterMapper implements BindParameterMapper<String[]> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<String[]> targetType() {
		return String[].class;
	}

	@Override
	public Object toJdbc(final String[] original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return JdbcParameterFactory.createArrayOf(connection, "VARCHAR", original);
	}
}
