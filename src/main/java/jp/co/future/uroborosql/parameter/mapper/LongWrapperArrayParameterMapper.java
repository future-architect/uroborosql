package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;

/**
 * {@link Long}配列用{@link BindParameterMapper}
 *
 * @author H.Sugimoto
 */
public class LongWrapperArrayParameterMapper implements BindParameterMapper<Long[]> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<Long[]> targetType() {
		return Long[].class;
	}

	@Override
	public Object toJdbc(final Long[] original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return JdbcParameterFactory.createArrayOf(connection, "BIGINT", original);
	}
}
