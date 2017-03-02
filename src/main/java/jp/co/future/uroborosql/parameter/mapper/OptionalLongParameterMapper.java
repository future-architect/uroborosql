package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;
import java.util.OptionalLong;

/**
 * {@link OptionalLong}用{@link BindParameterMapper}
 *
 * @author ota
 */
public class OptionalLongParameterMapper implements BindParameterMapper<OptionalLong> {
	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<OptionalLong> targetType() {
		return OptionalLong.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final OptionalLong original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return original.isPresent() ? original.getAsLong() : null;
	}
}
