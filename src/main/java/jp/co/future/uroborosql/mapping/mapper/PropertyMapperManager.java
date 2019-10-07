/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping.mapper;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.Clock;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.ServiceLoader;
import java.util.concurrent.CopyOnWriteArrayList;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * プロパティ変換クラス<br>
 * Entityに渡す値を調整する
 *
 * @author ota
 */
public final class PropertyMapperManager {
	/** デフォルトMapper */
	private static final PropertyMapper<?>[] DEFAULT_MAPPERS = {
			new BigIntegerPropertyMapper(),
			new OptionalPropertyMapper(),
			new OptionalIntPropertyMapper(),
			new OptionalLongPropertyMapper(),
			new OptionalDoublePropertyMapper(),
			new DomainPropertyMapper(),
			new EnumPropertyMapper(), // DomainPropertyMapper・DateTimeApiPropertyMapperより後に設定
			new ArrayPropertyMapper()
	};

	/** Serviceに登録されたMapper */
	private static final List<PropertyMapper<?>> LOADED_MAPPERS = load();

	private static List<PropertyMapper<?>> load() {
		List<PropertyMapper<?>> list = new ArrayList<>();
		ServiceLoader.load(PropertyMapper.class).forEach(list::add);
		return list;
	}

	/** プロパティMapperのリスト */
	private final List<PropertyMapper<?>> mappers;

	/** dateTimeApiPropertyMapper */
	private final DateTimeApiPropertyMapper dateTimeApiPropertyMapper;

	/**
	 * コンストラクタ
	 *
	 * @param clock dateTimeApiPropertyMapper で使用するClock
	 */
	public PropertyMapperManager(final Clock clock) {
		this.mappers = new CopyOnWriteArrayList<>(LOADED_MAPPERS);
		this.dateTimeApiPropertyMapper = new DateTimeApiPropertyMapper(clock);
	}

	/**
	 * コピーコンストラクタ
	 *
	 * @param parameterMapperManager コピー元のパラメータ変換クラス
	 * @param clock dateTimeApiPropertyMapper で使用するClock
	 */
	public PropertyMapperManager(final PropertyMapperManager parameterMapperManager, final Clock clock) {
		this.mappers = new CopyOnWriteArrayList<>(parameterMapperManager.mappers);
		this.dateTimeApiPropertyMapper = new DateTimeApiPropertyMapper(clock);
	}

	/**
	 * {@link PropertyMapper}を追加
	 *
	 * @param propertyMapper {@link PropertyMapper}
	 */
	public void addMapper(final PropertyMapper<?> propertyMapper) {
		this.mappers.add(propertyMapper);
	}

	/**
	 * {@link PropertyMapper}をremove
	 *
	 * @param propertyMapper {@link PropertyMapper}
	 */
	public void removeMapper(final PropertyMapper<?> propertyMapper) {
		this.mappers.remove(propertyMapper);
	}

	/**
	 * プロパティの型に変換した値を取得
	 *
	 * @param type 変換対象の型
	 * @param rs ResultSet
	 * @param columnIndex 変換対象データのカラムIndex
	 * @return プロパティの型に変換した値
	 * @throws SQLException SQL例外
	 */
	public Object getValue(final JavaType type, final ResultSet rs, final int columnIndex) throws SQLException {
		Class<?> rawType = type.getRawType();
		for (PropertyMapper<?> propertyMapper : this.mappers) {
			if (propertyMapper.canAccept(rawType) && propertyMapper.canAcceptTest(type, rs, columnIndex, this)) {
				return propertyMapper.getValue(type, rs, columnIndex, this);
			}
		}

		if (String.class.equals(rawType)) {
			return rs.getString(columnIndex);
		}
		if (Boolean.class.equals(rawType)) {
			boolean result = rs.getBoolean(columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return result;
		}
		if (boolean.class.equals(rawType)) {
			return rs.getBoolean(columnIndex);
		}
		if (Byte.class.equals(rawType)) {
			byte result = rs.getByte(columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return result;
		}
		if (byte.class.equals(rawType)) {
			return rs.getByte(columnIndex);
		}
		if (Short.class.equals(rawType)) {
			short result = rs.getShort(columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return result;
		}
		if (short.class.equals(rawType)) {
			return rs.getShort(columnIndex);
		}
		if (Integer.class.equals(rawType)) {
			int result = rs.getInt(columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return result;
		}
		if (int.class.equals(rawType)) {
			return rs.getInt(columnIndex);
		}
		if (Long.class.equals(rawType)) {
			long result = rs.getLong(columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return result;
		}
		if (long.class.equals(rawType)) {
			return rs.getLong(columnIndex);
		}
		if (Float.class.equals(rawType)) {
			float result = rs.getFloat(columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return result;
		}
		if (float.class.equals(rawType)) {
			return rs.getFloat(columnIndex);
		}
		if (Double.class.equals(rawType)) {
			double result = rs.getDouble(columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return result;
		}
		if (double.class.equals(rawType)) {
			return rs.getDouble(columnIndex);
		}
		if (BigDecimal.class.equals(rawType)) {
			return rs.getBigDecimal(columnIndex);
		}
		if (byte[].class.equals(rawType)) {
			return rs.getBytes(columnIndex);
		}
		if (java.sql.Timestamp.class.equals(rawType)) {
			return rs.getTimestamp(columnIndex);
		}
		if (java.sql.Time.class.equals(rawType)) {
			return rs.getTime(columnIndex);
		}
		if (java.sql.Date.class.equals(rawType)) {
			return rs.getDate(columnIndex);
		}
		if (Date.class.equals(rawType)) {
			return rs.getTimestamp(columnIndex);
		}
		if (java.sql.Array.class.equals(rawType)) {
			return rs.getArray(columnIndex);
		}
		if (java.sql.Blob.class.equals(rawType)) {
			return rs.getBlob(columnIndex);
		}
		if (java.sql.Clob.class.equals(rawType)) {
			return rs.getClob(columnIndex);
		}
		if (java.sql.NClob.class.equals(rawType)) {
			return rs.getNClob(columnIndex);
		}
		if (java.sql.Ref.class.equals(rawType)) {
			return rs.getRef(columnIndex);
		}
		if (java.sql.SQLXML.class.equals(rawType)) {
			return rs.getSQLXML(columnIndex);
		}
		if (dateTimeApiPropertyMapper.canAccept(rawType)
				&& dateTimeApiPropertyMapper.canAcceptTest(type, rs, columnIndex, this)) {
			return dateTimeApiPropertyMapper.getValue(type, rs, columnIndex, this);
		}
		for (PropertyMapper<?> propertyMapper : DEFAULT_MAPPERS) {
			if (propertyMapper.canAccept(rawType) && propertyMapper.canAcceptTest(type, rs, columnIndex, this)) {
				return propertyMapper.getValue(type, rs, columnIndex, this);
			}
		}
		return rs.getObject(columnIndex);
	}
}
