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
import java.time.Clock;
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
import java.time.chrono.ChronoLocalDate;
import java.time.chrono.Era;
import java.time.temporal.TemporalAccessor;
import java.util.Calendar;
import java.util.Optional;
import java.util.TimeZone;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.JavaType;

/**
 * Date and Time API用{@link PropertyMapper}
 *
 * @author ota
 */
public class DateTimeApiPropertyMapper implements PropertyMapper<TemporalAccessor> {
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
			return Optional.ofNullable(rs.getTimestamp(columnIndex))
					.map(java.sql.Timestamp::toLocalDateTime)
					.orElse(null);
		}
		if (OffsetDateTime.class.equals(rawType)) {
			return Optional.ofNullable(rs.getTimestamp(columnIndex))
					.map(java.sql.Timestamp::toLocalDateTime)
					.map(d -> OffsetDateTime.of(d, OffsetDateTime.now(clock).getOffset()))
					.orElse(null);
		}
		if (ZonedDateTime.class.equals(rawType)) {
			return Optional.ofNullable(rs.getTimestamp(columnIndex))
					.map(java.sql.Timestamp::toLocalDateTime)
					.map(d -> ZonedDateTime.of(d, clock.getZone()))
					.orElse(null);
		}
		if (LocalDate.class.equals(rawType)) {
			return Optional.ofNullable(rs.getDate(columnIndex))
					.map(java.sql.Date::toLocalDate)
					.orElse(null);
		}
		if (LocalTime.class.equals(rawType)) {
			return Optional.ofNullable(rs.getTime(columnIndex))
					.map(this::sqlTimeToLocalTime)
					.orElse(null);
		}
		if (OffsetTime.class.equals(rawType)) {
			return Optional.ofNullable(rs.getTime(columnIndex))
					.map(this::sqlTimeToOffsetTime)
					.orElse(null);
		}

		if (Year.class.equals(rawType)) {
			int value = rs.getInt(columnIndex);
			return rs.wasNull() ? null : Year.of(value);
		}
		if (YearMonth.class.equals(rawType)) {
			int value = rs.getInt(columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return YearMonth.of(value / 100, value % 100);
		}
		if (MonthDay.class.equals(rawType)) {
			int value = rs.getInt(columnIndex);
			if (rs.wasNull()) {
				return null;
			}
			return MonthDay.of(value / 100, value % 100);
		}
		if (Month.class.equals(rawType)) {
			int value = rs.getInt(columnIndex);
			return rs.wasNull() ? null : Month.of(value);
		}
		if (DayOfWeek.class.equals(rawType)) {
			int value = rs.getInt(columnIndex);
			return rs.wasNull() ? null : DayOfWeek.of(value);
		}
		if (Era.class.isAssignableFrom(rawType)) {
			int value = rs.getInt(columnIndex);
			try {
				return rs.wasNull() ? null : (Era) rawType.getMethod("of", int.class).invoke(null, value);
			} catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				throw new UroborosqlRuntimeException(e);
			}
		}

		// JapaneseDate等のChronoLocalDateの変換
		if (ChronoLocalDate.class.isAssignableFrom(rawType)) {
			java.sql.Date date = rs.getDate(columnIndex);
			if (date == null) {
				return null;
			}
			LocalDate localDate = date.toLocalDate();

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
	 * {@link java.sql.Time} から {@link OffsetTime} への変換を行う.
	 *
	 * @param time 変換する{@link java.sql.Time}
	 * @return 変換後の{@link OffsetTime}
	 */
	private OffsetTime sqlTimeToOffsetTime(final java.sql.Time time) {
		long milliseconds = time.getTime();
		Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone(clock.getZone()));
		calendar.setTimeInMillis(milliseconds);
		return OffsetDateTime.ofInstant(calendar.toInstant(), clock.getZone()).toOffsetTime();
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
