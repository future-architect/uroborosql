package jp.co.future.uroborosql.mapping.mapper;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.sql.ResultSet;
import java.sql.SQLException;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.JavaType;
import jp.co.future.uroborosql.mapping.annotations.Domain;

import org.apache.commons.lang3.StringUtils;

/**
 * {@link Domain}用{@link PropertyMapper}
 *
 * @author ota
 */
public class DomainPropertyMapper implements PropertyMapper<Object> {

	@Override
	public boolean canAccept(final Class<?> type) {
		return type.getAnnotation(Domain.class) != null;
	}

	@Override
	public Object getValue(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager) throws SQLException {
		Class<?> rawType = type.getRawType();
		Domain domain = rawType.getAnnotation(Domain.class);
		Object value = mapperManager.getValue(JavaType.of(domain.valueType()), rs, columnIndex);

		if (value == null) {
			if (!domain.nullable()) {
				return null;
			}
		}
		return toDomain(rawType, domain, value);

	}

	private Object toDomain(final Class<?> type, final Domain domain, final Object value) {
		try {
			String methodName = domain.factoryMethod();
			if (StringUtils.isBlank(methodName)) {
				// default
				if (!type.isEnum()) {
					return type.getConstructor(domain.valueType()).newInstance(value);
				} else {
					// Enumの場合valueOfメソッドをcallする
					methodName = "valueOf";
				}
			}
			Method method = type.getMethod(methodName, domain.valueType());
			if (!Modifier.isStatic(method.getModifiers())) {
				throw new IllegalStateException("not static method. [" + type.getSimpleName() + "#" + methodName + "]");
			} else if (!type.isAssignableFrom(method.getReturnType())) {
				throw new IllegalStateException("unmatch method result type. [" + type.getSimpleName() + "#" + methodName + "]");
			}
			return method.invoke(null, value);
		} catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException
				| SecurityException e) {
			throw new UroborosqlRuntimeException(e);
		}
	}

}
