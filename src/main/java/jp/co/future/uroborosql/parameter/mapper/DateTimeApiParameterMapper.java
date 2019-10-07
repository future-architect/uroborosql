/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;
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
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalAccessor;
import java.util.Arrays;
import java.util.Calendar;
import java.util.TimeZone;

/**
 * Date and Time API用{@link BindParameterMapper}
 *
 * @author ota
 */
public class DateTimeApiParameterMapper implements BindParameterMapper<TemporalAccessor> {
	/**
	 * 標準時間フィールド群
	 */
	private static final ChronoField[] STANDARD_TARGET = {
			ChronoField.MILLI_OF_SECOND,
			ChronoField.SECOND_OF_MINUTE,
			ChronoField.MINUTE_OF_HOUR,
			ChronoField.HOUR_OF_DAY,
			ChronoField.DAY_OF_MONTH,
			ChronoField.MONTH_OF_YEAR,
			ChronoField.YEAR, };

	/**
	 * 日付対象時間フィールド群
	 */
	private static final ChronoField[] DATE_TARGET = Arrays.stream(ChronoField.values())
			.filter(ChronoField::isDateBased).toArray(ChronoField[]::new);

	/**
	 * 時間対象時間フィールド群
	 */
	private static final ChronoField[] TIME_TARGET = Arrays.stream(ChronoField.values())
			.filter(ChronoField::isTimeBased).toArray(ChronoField[]::new);

	/** 日時の変換に使用するClock */
	private final Clock clock;

	public DateTimeApiParameterMapper(final Clock clock) {
		this.clock = clock;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<TemporalAccessor> targetType() {
		return TemporalAccessor.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final TemporalAccessor original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {

		/* Dateに変換 */
		if (original instanceof LocalDateTime) {
			return java.sql.Timestamp.valueOf((LocalDateTime) original);
		}
		if (original instanceof OffsetDateTime) {
			return new java.sql.Timestamp(((OffsetDateTime) original).toInstant().toEpochMilli());
		}
		if (original instanceof ZonedDateTime) {
			return new java.sql.Timestamp(((ZonedDateTime) original).toInstant().toEpochMilli());
		}
		if (original instanceof LocalDate) {
			return java.sql.Date.valueOf((LocalDate) original);
		}
		if (original instanceof LocalTime) {
			return new java.sql.Time(toTime(original));
		}
		if (original instanceof OffsetTime) {
			Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone(clock.getZone()));
			int year = calendar.get(Calendar.YEAR);
			long thisYearDate = ((OffsetTime) original).atDate(LocalDate.of(year, Month.JANUARY, 1)).toInstant()
					.toEpochMilli();
			calendar.setTimeInMillis(thisYearDate);
			//yearを引いて1970年にする
			calendar.add(Calendar.YEAR, 1970 - year);
			return new java.sql.Time(calendar.getTimeInMillis());
		}

		/* 数値に変換 */
		if (original instanceof Year) {
			return ((Year) original).getValue();
		}
		if (original instanceof YearMonth) {
			YearMonth yearMonth = (YearMonth) original;
			return yearMonth.getYear() * 100 + yearMonth.getMonthValue();
		}
		if (original instanceof MonthDay) {
			MonthDay monthDay = (MonthDay) original;
			return monthDay.getMonthValue() * 100 + monthDay.getDayOfMonth();
		}
		if (original instanceof Month) {
			return ((Month) original).getValue();
		}

		if (original instanceof DayOfWeek) {
			return ((DayOfWeek) original).getValue();
		}
		if (original instanceof Era) {
			return ((Era) original).getValue();
		}

		// JapaneseDate等のChronoLocalDateの変換 Dateに変換
		if (original instanceof ChronoLocalDate) {
			return new java.sql.Date(((ChronoLocalDate) original).atTime(LocalTime.MIDNIGHT)
					.atZone(clock.getZone()).toInstant().toEpochMilli());
		}

		// その他の型
		if (original instanceof Instant) {
			return new java.sql.Timestamp(((Instant) original).toEpochMilli());
		}

		if (isCastTarget(original)) {
			boolean incDate = Arrays.stream(DATE_TARGET).anyMatch(original::isSupported);
			boolean incTime = Arrays.stream(TIME_TARGET).anyMatch(original::isSupported);
			if (incDate && incTime) {
				return new java.sql.Timestamp(toTime(original));
			} else if (incDate) {
				return new java.sql.Date(toTime(original));
			} else {
				return new java.sql.Time(toTime(original));
			}
		}

		return original;
	}

	/**
	 * 時間的オブジェクトへキャスト可能かどうかを判定する
	 * @param temporalAccessor 時間的オブジェクト
	 * @return キャスト可能な場合は<code>true</code>
	 */
	private boolean isCastTarget(final TemporalAccessor temporalAccessor) {
		return Arrays.stream(STANDARD_TARGET).anyMatch(temporalAccessor::isSupported);
	}

	/**
	 * 時間への変換
	 *
	 * @param temporalAccessor 時間的オブジェクト
	 * @return カレンダー時間のミリ秒
	 */
	private long toTime(final TemporalAccessor temporalAccessor) {
		Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone(clock.getZone()));
		setField(calendar, Calendar.YEAR, temporalAccessor, ChronoField.YEAR, 0, 1970);
		setField(calendar, Calendar.MONTH, temporalAccessor, ChronoField.MONTH_OF_YEAR, -1, 0);
		setField(calendar, Calendar.DATE, temporalAccessor, ChronoField.DAY_OF_MONTH, 0, 1);
		setField(calendar, Calendar.HOUR_OF_DAY, temporalAccessor, ChronoField.HOUR_OF_DAY, 0, 0);
		setField(calendar, Calendar.MINUTE, temporalAccessor, ChronoField.MINUTE_OF_HOUR, 0, 0);
		setField(calendar, Calendar.SECOND, temporalAccessor, ChronoField.SECOND_OF_MINUTE, 0, 0);
		setField(calendar, Calendar.MILLISECOND, temporalAccessor, ChronoField.MILLI_OF_SECOND, 0, 0);
		return calendar.getTimeInMillis();
	}

	/**
	 * カレンダーへの値の設定
	 *
	 * 時間的オブジェクトが時間フィールドをサポートしている場合はchoronoFieldとoffsetから算出した値を設定する。
	 * サポートしていない場合は、設定値を設定する。
	 *
	 * @param calendar 対象のカレンダー
	 * @param field 対象フィールド
	 * @param temporalAccessor 時間的オブジェクト
	 * @param chronoField 時間フィールド
	 * @param offset オフセット
	 * @param def 設定値
	 */
	private void setField(final Calendar calendar, final int field, final TemporalAccessor temporalAccessor,
			final ChronoField chronoField, final int offset, final int def) {
		calendar.set(field, temporalAccessor.isSupported(chronoField) ? temporalAccessor.get(chronoField) + offset
				: def);
	}
}
