package jp.co.future.uroborosql.mapping.mapper;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.OptionalLong;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * {@link OptionalLong}用{@link PropertyMapper}
 *
 * @author ota
 */
public class OptionalLongPropertyMapper implements PropertyMapper<OptionalLong> {
	@Override
	public boolean canAccept(final Class<?> type) {
		return OptionalLong.class.equals(type);
	}

	@Override
	public OptionalLong getValue(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager) throws SQLException {
		long value = rs.getLong(columnIndex);
		if (!rs.wasNull()) {
			return OptionalLong.of(value);
		} else {
			return OptionalLong.empty();
		}
	}
}
