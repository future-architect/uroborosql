/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.converter;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.MonthDay;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Year;
import java.time.YearMonth;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.OptionalLong;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.JavaType;
import jp.co.future.uroborosql.mapping.annotations.Domain;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.ObjectUtils;

/**
 * 検索結果をプリミティブ型（のラッパークラス）に変換する変換器
 *
 * @param <E> プリミティブ型のラッパークラス、またはString型
 * @author H.Sugimoto
 *
 */
public class ScalarResultSetConverter<E> implements ResultSetConverter<E> {
	private final String col;
	private final JavaType javaType;
	private final PropertyMapperManager mapperManager;
	private int columnPosition;

	/**
	 * 受付可能な型かどうかを判定する.
	 *
	 * @param type 判定対象の型
	 * @return 受付可能な場合<code>true</code> を返す.
	 */
	public static final boolean accept(final Class<?> type) {
		if (type == null) {
			return false;
		} else if ( // 基本的な型は受付可能とする
		String.class.equals(type) ||
				boolean.class.equals(type) ||
				Boolean.class.equals(type) ||
				byte.class.equals(type) ||
				Byte.class.equals(type) ||
				short.class.equals(type) ||
				Short.class.equals(type) ||
				int.class.equals(type) ||
				Integer.class.equals(type) ||
				long.class.equals(type) ||
				Long.class.equals(type) ||
				float.class.equals(type) ||
				Float.class.equals(type) ||
				double.class.equals(type) ||
				Double.class.equals(type) ||
				BigInteger.class.equals(type) ||
				BigDecimal.class.equals(type) ||
				type.isEnum() ||
				// Date型かDate型を継承するクラス（Timestampなど）は受付可能とする
				Date.class.equals(type) ||
				java.sql.Date.class.equals(type) ||
				java.sql.Time.class.equals(type) ||
				java.sql.Timestamp.class.equals(type) ||
				// jdbcの提供するカラム型は受付可能とする
				java.sql.Array.class.equals(type) ||
				java.sql.Clob.class.equals(type) ||
				java.sql.NClob.class.equals(type) ||
				java.sql.Blob.class.equals(type) ||
				java.sql.Ref.class.equals(type) ||
				java.sql.SQLXML.class.equals(type) ||
				// java.time配下の時間オブジェクトは受付可能とする
				LocalDateTime.class.equals(type) ||
				OffsetDateTime.class.equals(type) ||
				ZonedDateTime.class.equals(type) ||
				LocalDate.class.equals(type) ||
				LocalTime.class.equals(type) ||
				OffsetTime.class.equals(type) ||
				Year.class.equals(type) ||
				YearMonth.class.equals(type) ||
				MonthDay.class.equals(type) ||
				Month.class.equals(type) ||
				DayOfWeek.class.equals(type) ||
				// Optional（とその特殊型）は受付可能とする
				Optional.class.equals(type) ||
				OptionalInt.class.equals(type) ||
				OptionalLong.class.equals(type) ||
				OptionalDouble.class.equals(type) ||
				// 配列型でコンポーネント型が受入対象の型である場合は受入可能とする
				Object[].class.equals(type) ||
				byte[].class.equals(type) ||
				// Domain型は受入可能とする
				type.getAnnotation(Domain.class) != null) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * コンストラクタ
	 *
	 * @param col 取得対象のカラム名. null の場合は先頭カラムを取得
	 * @param columnType カラムの型
	 * @param mapperManager PropertyMapperManager
	 */
	public ScalarResultSetConverter(final String col, final Class<? extends E> columnType,
			final PropertyMapperManager mapperManager) {
		this.col = CaseFormat.UPPER_SNAKE_CASE.convert(col);
		this.javaType = JavaType.of(columnType);
		this.mapperManager = mapperManager;
		this.columnPosition = ObjectUtils.isEmpty(col) ? 1 : -1;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.converter.ResultSetConverter#createRecord(java.sql.ResultSet)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public E createRecord(final ResultSet rs) throws SQLException {
		try {
			if (this.columnPosition == -1) {
				var rsmd = rs.getMetaData();
				var columnCount = rsmd.getColumnCount();

				// 指定されたカラムのpositionを取得
				for (var i = 1; i <= columnCount; i++) {
					var columnLabel = CaseFormat.UPPER_SNAKE_CASE.convert(rsmd.getColumnLabel(i));
					if (col.equalsIgnoreCase(columnLabel)) {
						this.columnPosition = i;
						break;
					}
				}
				if (this.columnPosition == -1) {
					// 指定されたカラムが見つからない場合は例外をスローする
					throw new UroborosqlRuntimeException(col + " not found in query result.");
				}
			}

			return (E) mapperManager.getValue(this.javaType, rs, this.columnPosition);
		} catch (SQLException | RuntimeException | Error ex) {
			throw ex;
		}
	}
}
