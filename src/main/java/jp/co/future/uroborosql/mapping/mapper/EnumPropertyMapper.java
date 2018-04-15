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
 * {@link Enum}用{@link PropertyMapper}<br>
 * EnumのtoString()に変換
 *
 * @author ota
 */
public class EnumPropertyMapper implements PropertyMapper<Enum<?>> {

	@Override
	public boolean canAccept(final Class<?> type) {
		return type.isEnum();
	}

	@Override
	public Enum<?> getValue(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager) throws SQLException {
		String name = rs.getString(columnIndex);
		if (name == null) {
			return null;
		}
		Class<?> rawType = type.getRawType();
		Object[] enums = rawType.getEnumConstants();
		for (Object object : enums) {
			if (object.toString().equals(name)) {
				return (Enum<?>) object;
			}
		}
		throw new IllegalArgumentException(
				"No enum constant " + rawType.getCanonicalName() + "." + name);
	}

}
