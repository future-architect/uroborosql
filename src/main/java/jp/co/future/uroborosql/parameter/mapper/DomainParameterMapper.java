package jp.co.future.uroborosql.parameter.mapper;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
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
		Object value = getValue(type, domain, original);

		return parameterMapperManager.toJdbc(value, connection);
	}

	private Object getValue(final Class<?> type, final Domain domain, final Object original) {
		String methodName = domain.toJdbcMethod();
		try {
			try {
				Method method = type.getMethod(methodName);
				if (Modifier.isStatic(method.getModifiers())) {
					throw new IllegalStateException("not class method. [" + type.getSimpleName() + "#" + methodName + "]");
				} else if (!domain.valueType().isAssignableFrom(method.getReturnType())) {
					throw new IllegalStateException("unmatch method result type. [" + type.getSimpleName() + "#" + methodName + "]");
				}
				return method.invoke(original);
			} catch (NoSuchMethodException e) {
				try {
					// static?
					Method method = type.getMethod(methodName, type);
					if (!Modifier.isStatic(method.getModifiers())) {
						throw new IllegalStateException("not static method. [" + type.getSimpleName() + "#" + methodName + "]");
					} else if (!domain.valueType().isAssignableFrom(method.getReturnType())) {
						throw new IllegalStateException("unmatch method result type. [" + type.getSimpleName() + "#" + methodName + "]");
					}
					return method.invoke(null, original);
				} catch (NoSuchMethodException e2) {
					throw new UroborosqlRuntimeException(e);// 元のエラーでthrow
				}
			}
		} catch (IllegalAccessException | InvocationTargetException | SecurityException e) {
			throw new UroborosqlRuntimeException(e);
		}
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
