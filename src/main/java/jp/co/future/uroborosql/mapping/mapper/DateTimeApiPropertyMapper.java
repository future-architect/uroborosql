/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping.mapper;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.time.Clock;
import java.time.DayOfWeek;
import java.time.Instant;
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
import java.time.chrono.ChronoLocalDate;
import java.time.chrono.Era;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalAccessor;
import java.util.Calendar;
import java.util.Optional;
import java.util.TimeZone;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.JavaType;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * Date and Time API用{@link PropertyMapper}
 *
 * @author ota
 */
public class DateTimeApiPropertyMapper implements PropertyMapper<TemporalAccessor> {
	/**
	 * yyyyMMddHHmmss文字列からLocalDateTimeに変換するためのフォーマッター.
	 */
	private static final DateTimeFormatter FORMATTER_SHORT_DATE_TIME = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

	/**
	 * yyyyMMddHHmmssSSS文字列からLocalDateTimeに変換するためのフォーマッター.<br>
	 * Java8での不具合のため、DateTimeFormatterはBuilderを使用して生成する.
	 */
	private static final DateTimeFormatter FORMATTER_SHORT_DATE_TIME_WITH_MILLS = new DateTimeFormatterBuilder()
			.appendPattern("yyyyMMddHHmmss").appendValue(ChronoField.MILLI_OF_SECOND, 3).toFormatter();

	/**
	 * HHmm文字列からLocalTimeに変換するためのフォーマッター.
	 */
	private static final DateTimeFormatter FORMATTER_SHORT_TIME_WITHOUT_SEC = DateTimeFormatter.ofPattern("HHmm");

	/**
	 * HHmmss文字列からLocalTimeに変換するためのフォーマッター.
	 */
	private static final DateTimeFormatter FORMATTER_SHORT_TIME = DateTimeFormatter.ofPattern("HHmmss");

	/**
	 * HHmmssSSS文字列からLocalTimeに変換するためのフォーマッター.
	 */
	private static final DateTimeFormatter FORMATTER_SHORT_TIME_WITH_MILLS = DateTimeFormatter.ofPattern("HHmmssSSS");

	/**
	 * 日時の変換に使用するClock
	 */
	private final Clock clock;

	/**
	 * コンストラクタ.
	 *
	 * @param clock 日時の変換に使用するClock
	 */
	public DateTimeApiPropertyMapper(final Clock clock) {
		super();
		this.clock = clock;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.mapper.PropertyMapper#canAccept(java.lang.Class)
	 */
	@Override
	public boolean canAccept(final Class<?> type) {
		return LocalDateTime.class.equals(type)
				|| OffsetDateTime.class.equals(type)
				|| ZonedDateTime.class.equals(type)
				|| LocalDate.class.equals(type)
				|| LocalTime.class.equals(type)
				|| OffsetTime.class.equals(type)
				|| Year.class.equals(type)
				|| YearMonth.class.equals(type)
				|| MonthDay.class.equals(type)
				|| Month.class.equals(type)
				|| DayOfWeek.class.equals(type)
				|| checkEra(type)
				|| checkChronoLocalDate(type);
	}

	private boolean checkChronoLocalDate(final Class<?> type) {
		if (!ChronoLocalDate.class.isAssignableFrom(type)) {
			return false;
		}
		try {
			Method method = type.getMethod("of", int.class, int.class, int.class);
			return Modifier.isStatic(method.getModifiers())
					&& Modifier.isPublic(method.getModifiers())
					&& method.getReturnType().equals(type);
		} catch (NoSuchMethodException | SecurityException e) {
			return false;
		}
	}

	private boolean checkEra(final Class<?> type) {
		if (!Era.class.isAssignableFrom(type)) {
			return false;
		}
		try {
			Method method = type.getMethod("of", int.class);
			return Modifier.isStatic(method.getModifiers())
					&& Modifier.isPublic(method.getModifiers())
					&& method.getReturnType().equals(type);
		} catch (NoSuchMethodException | SecurityException e) {
			return false;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.mapper.PropertyMapper#getValue(jp.co.future.uroborosql.mapping.JavaType, java.sql.ResultSet, int, jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager)
	 */
	@Override
	public TemporalAccessor getValue(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager) throws SQLException {
		Class<?> rawType = type.getRawType();
		if (LocalDateTime.class.equals(rawType)) {
			return getLocalDateTime(rs, columnIndex)
					.orElse(null);
		}
		if (OffsetDateTime.class.equals(rawType)) {
			return getLocalDateTime(rs, columnIndex)
					.map(d -> OffsetDateTime.of(d, OffsetDateTime.now(clock).getOffset()))
					.orElse(null);
		}
		if (ZonedDateTime.class.equals(rawType)) {
			return getLocalDateTime(rs, columnIndex)
					.map(d -> ZonedDateTime.of(d, clock.getZone()))
					.orElse(null);
		}
		if (LocalDate.class.equals(rawType)) {
			return getLocalDate(rs, columnIndex)
					.orElse(null);
		}
		if (LocalTime.class.equals(rawType)) {
			return getLocalTime(rs, columnIndex)
					.orElse(null);
		}
		if (OffsetTime.class.equals(rawType)) {
			return getLocalTime(rs, columnIndex)
					.map(time -> OffsetTime.of(time, clock.getZone().getRules().getOffset(Instant.EPOCH)))
					.orElse(null);
		}

		if (Year.class.equals(rawType)) {
			int value = getInt(rs, columnIndex);
			return rs.wasNull() ? null : Year.of(value);
		}
		if (YearMonth.class.equals(rawType)) {
			int value = getInt(rs, columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return YearMonth.of(value / 100, value % 100);
		}
		if (MonthDay.class.equals(rawType)) {
			int value = getInt(rs, columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return MonthDay.of(value / 100, value % 100);
		}
		if (Month.class.equals(rawType)) {
			int value = getInt(rs, columnIndex);
			return rs.wasNull() ? null : Month.of(value);
		}
		if (DayOfWeek.class.equals(rawType)) {
			int value = getInt(rs, columnIndex);
			return rs.wasNull() ? null : DayOfWeek.of(value);
		}
		if (Era.class.isAssignableFrom(rawType)) {
			int value = getInt(rs, columnIndex);
			try {
				return rs.wasNull() ? null : (Era) rawType.getMethod("of", int.class).invoke(null, value);
			} catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				throw new UroborosqlRuntimeException(e);
			}
		}

		// JapaneseDate等のChronoLocalDateの変換
		if (ChronoLocalDate.class.isAssignableFrom(rawType)) {
			LocalDate localDate = getLocalDate(rs, columnIndex).orElse(null);
			if (localDate == null) {
				return null;
			}

			try {
				return (ChronoLocalDate) rawType.getMethod("of", int.class, int.class, int.class).invoke(null,
						localDate.getYear(),
						localDate.getMonthValue(),
						localDate.getDayOfMonth());
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException
					| NoSuchMethodException | SecurityException e) {
				throw new UroborosqlRuntimeException(e);
			}
		}
		throw new UroborosqlRuntimeException();
	}

	/**
	 * ResultSetの指定したカラムの値からintを取得する.<br>
	 *
	 * カラムの型が文字列の場合はintへの変換を試みる.
	 *
	 * @param rs 検索結果
	 * @param columnIndex カラムインデックス
	 * @return 変換したint. カラムの値がnullまたは空文字の場合は0を返却
	 * @throws SQLException
	 */
	private int getInt(final ResultSet rs, final int columnIndex) throws SQLException {
		int columnType = rs.getMetaData().getColumnType(columnIndex);
		if (isStringType(columnType)) {
			String str = rs.getString(columnIndex);
			if (StringUtils.isEmpty(str)) {
				return 0;
			} else if (StringUtils.isNumeric(str)) {
				return Integer.parseInt(str);
			} else {
				throw new UroborosqlRuntimeException("Text '" + str + "' could not be parsed to integer.");
			}
		} else {
			return rs.getInt(columnIndex);
		}
	}

	/**
	 * ResultSetの指定したカラムの値から{@link LocalDate}オブジェクトを取得する.<br>
	 *
	 * カラムの型が文字列の場合は{@link LocalDate}への変換を試みる.
	 *
	 * @param rs 検索結果
	 * @param columnIndex カラムインデックス
	 * @return 変換した{@link LocalDate}
	 * @throws SQLException
	 */
	private Optional<LocalDate> getLocalDate(final ResultSet rs, final int columnIndex) throws SQLException {
		int columnType = rs.getMetaData().getColumnType(columnIndex);
		if (isStringType(columnType)) {
			return Optional.ofNullable(rs.getString(columnIndex))
					.map(str -> {
						try {
							if (StringUtils.isEmpty(str)) {
								return null;
							} else if (str.length() == 8) { // yyyyMMdd
								return LocalDate.parse(str, DateTimeFormatter.BASIC_ISO_DATE);
							} else { // yyyy-MM-dd
								return LocalDate.parse(str, DateTimeFormatter.ISO_LOCAL_DATE);
							}
						} catch (DateTimeParseException ex) {
							throw new UroborosqlRuntimeException(ex);
						}
					});
		} else {
			return Optional.ofNullable(rs.getDate(columnIndex))
					.map(java.sql.Date::toLocalDate);
		}
	}

	/**
	 * ResultSetの指定したカラムの値から{@link LocalDateTime}オブジェクトを取得する.<br>
	 *
	 * カラムの型が文字列の場合は{@link LocalDateTime}への変換を試みる.
	 *
	 * @param rs 検索結果
	 * @param columnIndex カラムインデックス
	 * @return 変換した{@link LocalDateTime}
	 * @throws SQLException
	 */
	private Optional<LocalDateTime> getLocalDateTime(final ResultSet rs, final int columnIndex) throws SQLException {
		int columnType = rs.getMetaData().getColumnType(columnIndex);
		if (isStringType(columnType)) {
			return Optional.ofNullable(rs.getString(columnIndex))
					.map(str -> {
						try {
							if (StringUtils.isEmpty(str)) {
								return null;
							} else if (str.length() == 14) { // yyyyMMddHHmmss
								return LocalDateTime.parse(str, FORMATTER_SHORT_DATE_TIME);
							} else if (str.length() == 17) { // yyyyMMddHHmmssSSS
								return LocalDateTime.parse(str, FORMATTER_SHORT_DATE_TIME_WITH_MILLS);
							} else { // yyyy-MM-ddTHH:mm:ss, yyyy-MM-ddTHH:mm:ss.SSS
								return LocalDateTime.parse(str, DateTimeFormatter.ISO_LOCAL_DATE_TIME);
							}
						} catch (DateTimeParseException ex) {
							throw new UroborosqlRuntimeException(ex);
						}
					});
		} else {
			return Optional.ofNullable(rs.getTimestamp(columnIndex))
					.map(Timestamp::toLocalDateTime);
		}
	}

	/**
	 * ResultSetの指定したカラムの値から{@link LocalTime}オブジェクトを取得する.<br>
	 *
	 * カラムの型が文字列の場合は{@link LocalTime}への変換を試みる.
	 *
	 * @param rs 検索結果
	 * @param columnIndex カラムインデックス
	 * @return 変換した{@link LocalTime}
	 * @throws SQLException
	 */
	private Optional<LocalTime> getLocalTime(final ResultSet rs, final int columnIndex) throws SQLException {
		int columnType = rs.getMetaData().getColumnType(columnIndex);
		if (isStringType(columnType)) {
			return Optional.ofNullable(rs.getString(columnIndex))
					.map(str -> {
						try {
							if (StringUtils.isEmpty(str)) {
								return null;
							} else if (str.length() == 4) { // HHmm
								return LocalTime.parse(str, FORMATTER_SHORT_TIME_WITHOUT_SEC);
							} else if (str.length() == 6) { // HHmmss
								return LocalTime.parse(str, FORMATTER_SHORT_TIME);
							} else if (str.length() == 9) { // HHmmssSSS
								return LocalTime.parse(str, FORMATTER_SHORT_TIME_WITH_MILLS);
							} else { // HH:mm, HH:mm:ss, HH:mm:ss.SSS
								return LocalTime.parse(str, DateTimeFormatter.ISO_LOCAL_TIME);
							}
						} catch (DateTimeParseException ex) {
							throw new UroborosqlRuntimeException(ex);
						}
					});
		} else {
			return Optional.ofNullable(rs.getTime(columnIndex))
					.map(this::sqlTimeToLocalTime);
		}
	}

	/**
	 * 文字列として扱えるカラム型を判定する
	 *
	 * @param columnType カラム型
	 * @return 文字列として扱える場合<code>true</code>
	 */
	private boolean isStringType(final int columnType) {
		return columnType == Types.CHAR ||
				columnType == Types.NCHAR ||
				columnType == Types.VARCHAR ||
				columnType == Types.NVARCHAR ||
				columnType == Types.LONGVARCHAR ||
				columnType == Types.LONGNVARCHAR;
	}

	/**
	 * {@link java.sql.Time} から {@link LocalTime} への変換を行う.
	 *
	 * @param time 変換する{@link java.sql.Time}
	 * @return 変換後の{@link LocalTime}
	 */
	private LocalTime sqlTimeToLocalTime(final java.sql.Time time) {
		long milliseconds = time.getTime();
		Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone(clock.getZone()));
		calendar.setTimeInMillis(milliseconds);
		return LocalDateTime.ofInstant(calendar.toInstant(), clock.getZone()).toLocalTime();
	}
}
