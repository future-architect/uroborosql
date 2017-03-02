package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;
import java.util.OptionalInt;

/**
 * {@link OptionalInt}用{@link BindParameterMapper}
 *
 * @author ota
 */
public class OptionalIntParameterMapper implements BindParameterMapper<OptionalInt> {
	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<OptionalInt> targetType() {
		return OptionalInt.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final OptionalInt original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return original.isPresent() ? original.getAsInt() : null;
	}
}
