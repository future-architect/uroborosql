/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping.mapper;

import java.math.BigInteger;
import java.sql.ResultSet;
import java.sql.SQLException;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * {@link BigInteger}用{@link PropertyMapper}
 *
 * @author ota
 */
public class BigIntegerPropertyMapper implements PropertyMapper<BigInteger> {

	@Override
	public boolean canAccept(final Class<?> type) {
		return BigInteger.class.equals(type);
	}

	@Override
	public BigInteger getValue(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager) throws SQLException {
		String s = rs.getString(columnIndex);
		return s != null ? new BigInteger(s) : null;
	}

}
