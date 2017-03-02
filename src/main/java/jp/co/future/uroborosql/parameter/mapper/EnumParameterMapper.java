package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;

/**
 * {@link Enum}用{@link BindParameterMapper}<br>
 * EnumのtoString()に変換
 *
 * @author ota
 */
public class EnumParameterMapper implements BindParameterMapper<Enum<?>> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public Class<Enum<?>> targetType() {
		return (Class) Enum.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final Enum<?> original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return original.toString();
	}
}
