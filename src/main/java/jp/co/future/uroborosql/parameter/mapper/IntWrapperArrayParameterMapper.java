package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;

/**
 * {@link Integer}配列用{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class IntWrapperArrayParameterMapper implements BindParameterMapper<Integer[]> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<Integer[]> targetType() {
		return Integer[].class;
	}

	@Override
	public Object toJdbc(final Integer[] original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return JdbcParameterFactory.createArrayOf(connection, "INTEGER", original);
	}
}
