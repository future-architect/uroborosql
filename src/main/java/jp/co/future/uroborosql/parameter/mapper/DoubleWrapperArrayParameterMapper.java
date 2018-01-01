package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;

/**
 * {@link Double}配列用{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class DoubleWrapperArrayParameterMapper implements BindParameterMapper<Double[]> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<Double[]> targetType() {
		return Double[].class;
	}

	@Override
	public Object toJdbc(final Double[] original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return JdbcParameterFactory.createArrayOf(connection, "FLOAT", original);
	}
}
