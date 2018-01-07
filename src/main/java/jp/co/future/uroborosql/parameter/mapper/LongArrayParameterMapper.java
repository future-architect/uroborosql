package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;
import java.util.Arrays;

/**
 * {@link long}配列用{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class LongArrayParameterMapper implements BindParameterMapper<long[]> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<long[]> targetType() {
		return long[].class;
	}

	@Override
	public Object toJdbc(final long[] original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return JdbcParameterFactory.createArrayOf(connection, "BIGINT", Arrays.stream(original).mapToObj(Long::valueOf)
				.toArray());
	}
}
