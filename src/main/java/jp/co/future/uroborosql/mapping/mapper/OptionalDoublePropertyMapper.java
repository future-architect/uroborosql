package jp.co.future.uroborosql.mapping.mapper;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.OptionalDouble;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * {@link OptionalDouble}用{@link PropertyMapper}
 *
 * @author ota
 */
public class OptionalDoublePropertyMapper implements PropertyMapper<OptionalDouble> {

	@Override
	public boolean canAccept(final Class<?> type) {
		return OptionalDouble.class.equals(type);
	}

	@Override
	public OptionalDouble getValue(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager)
			throws SQLException {
		double value = rs.getDouble(columnIndex);
		if (!rs.wasNull()) {
			return OptionalDouble.of(value);
		} else {
			return OptionalDouble.empty();
		}
	}

}
