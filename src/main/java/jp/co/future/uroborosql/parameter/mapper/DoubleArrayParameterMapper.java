package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;
import java.util.Arrays;

/**
 * {@link double}配列用{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class DoubleArrayParameterMapper implements BindParameterMapper<double[]> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<double[]> targetType() {
		return double[].class;
	}

	@Override
	public Object toJdbc(final double[] original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return JdbcParameterFactory.createArrayOf(connection, "FLOAT", Arrays.stream(original)
				.mapToObj(Double::valueOf).toArray());
	}
}
