package jp.co.future.uroborosql.parameter.mapper;

import java.lang.reflect.InvocationTargetException;
import java.sql.Connection;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.annotations.Domain;

/**
 * {@link Domain}配列用{@link BindParameterMapper}
 *
 * @author ota
 */
public class DomainParameterMapper implements BindParameterMapper<Object> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<Object> targetType() {
		return Object.class;
	}

	@Override
	public boolean canAccept(final Object object) {
		return getDomainType(object) != null;
	}

	@Override
	public Object toJdbc(final Object original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		Class<?> type = getDomainType(original);
		Domain domain = type.getAnnotation(Domain.class);
		Object value;
		try {
			value = type.getMethod(domain.toJdbcMethod()).invoke(original);
		} catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new UroborosqlRuntimeException(e);
		}
		return parameterMapperManager.toJdbc(value, connection);
	}

	private Class<?> getDomainType(final Object object) {
		Class<?> type = object.getClass();
		while (!type.equals(Object.class)) {
			if (type.getAnnotation(Domain.class) != null) {
				return type;
			}
			type = type.getSuperclass();
		}
		return null;
	}
}
