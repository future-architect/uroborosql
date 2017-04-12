package jp.co.future.uroborosql.mapping.mapper;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.OptionalInt;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * {@link OptionalInt}用{@link PropertyMapper}
 *
 * @author ota
 */
public class OptionalIntPropertyMapper implements PropertyMapper<OptionalInt> {
	@Override
	public boolean canAccept(final Class<?> type) {
		return OptionalInt.class.equals(type);
	}

	@Override
	public OptionalInt getValue(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager) throws SQLException {
		int value = rs.getInt(columnIndex);
		if (!rs.wasNull()) {
			return OptionalInt.of(value);
		} else {
			return OptionalInt.empty();
		}
	}
}
