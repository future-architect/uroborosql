/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping.mapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * 配列用{@link PropertyMapper}
 *
 * @author ota
 */
public class ArrayPropertyMapper implements PropertyMapper<Object> {

	@Override
	public boolean canAccept(final Class<?> type) {
		return type.isArray();
	}

	@Override
	public boolean canAcceptTest(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager) throws SQLException {
		java.sql.Array array = rs.getArray(columnIndex);
		if (array == null) {
			return true;
		}
		Object o = array.getArray();
		return type.getRawType().isInstance(o);
	}

	@Override
	public Object getValue(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager) throws SQLException {
		java.sql.Array array = rs.getArray(columnIndex);
		if (array == null) {
			return null;
		}
		return array.getArray();
	}

}
