/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping.mapper;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Optional;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * {@link Optional}用{@link PropertyMapper}
 *
 * @author ota
 */
public class OptionalPropertyMapper implements PropertyMapper<Optional<?>> {
	@Override
	public boolean canAccept(final Class<?> type) {
		return Optional.class.equals(type);
	}

	@Override
	public Optional<?> getValue(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager) throws SQLException {
		return Optional.ofNullable(mapperManager.getValue(type.getParam(0), rs, columnIndex));
	}

}
