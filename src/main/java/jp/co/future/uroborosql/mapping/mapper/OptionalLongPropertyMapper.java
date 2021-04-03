/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
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
		var value = rs.getLong(columnIndex);
		if (!rs.wasNull()) {
			return OptionalLong.of(value);
		} else {
			return OptionalLong.empty();
		}
	}
}
