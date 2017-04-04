package jp.co.future.uroborosql.mapping.mapper;

import java.lang.reflect.InvocationTargetException;
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
		try {
			if (StringUtils.isBlank(domain.factoryMethod())) {
				return rawType.getConstructor(domain.valueType()).newInstance(value);
			} else {
				return rawType.getMethod(domain.factoryMethod(), domain.valueType()).invoke(null, value);
			}
		} catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException
				| SecurityException e) {
			throw new UroborosqlRuntimeException(e);
		}
	}

}
