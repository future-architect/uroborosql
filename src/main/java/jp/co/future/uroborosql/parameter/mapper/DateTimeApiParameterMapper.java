/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;
import java.sql.Date;
import java.sql.Time;
import java.sql.Timestamp;
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
import java.time.temporal.TemporalField;
import java.util.Arrays;

/**
 * Date and Time API用{@link BindParameterMapper}
 *
 * @author ota
 */
public class DateTimeApiParameterMapper implements BindParameterMapperWithClock<TemporalAccessor> {

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
			.filter(ChronoField::isDateBased)
			.toArray(ChronoField[]::new);

	/**
	 * 時間対象時間フィールド群
	 */
	private static final ChronoField[] TIME_TARGET = Arrays.stream(ChronoField.values())
			.filter(ChronoField::isTimeBased)
			.toArray(ChronoField[]::new);

	/**
	 * 日時の変換に使用するClock
	 */
	private Clock clock;

	/**
	 * コンストラクタ.
	 *
	 * @param clock Clock
	 */
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

	@Override
	public Clock getClock() {
		return clock;
	}

	@Override
	public void setClock(final Clock clock) {
		this.clock = clock;
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
			return Timestamp.valueOf((LocalDateTime) original);
		}
		if (original instanceof OffsetDateTime) {
			return Timestamp.from(((OffsetDateTime) original).toInstant());
		}
		if (original instanceof ZonedDateTime) {
			return Timestamp.from(((ZonedDateTime) original).toInstant());
		}
		if (original instanceof LocalDate) {
			return Date.valueOf((LocalDate) original);
		}
		if (original instanceof LocalTime) {
			return new Time(toEpochMilli(original));
		}
		if (original instanceof OffsetTime) {
			return new Time(toEpochMilli(original));
		}

		/* 数値に変換 */
		if (original instanceof Year) {
			return ((Year) original).getValue();
		}
		if (original instanceof YearMonth) {
			var yearMonth = (YearMonth) original;
			return yearMonth.getYear() * 100 + yearMonth.getMonthValue();
		}
		if (original instanceof MonthDay) {
			var monthDay = (MonthDay) original;
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
			return new Date(((ChronoLocalDate) original).atTime(LocalTime.MIDNIGHT)
					.atZone(getClock().getZone()).toInstant().toEpochMilli());
		}

		// その他の型
		if (original instanceof Instant) {
			return new Timestamp(((Instant) original).toEpochMilli());
		}

		if (isCastTarget(original)) {
			var incDate = Arrays.stream(DATE_TARGET).anyMatch(original::isSupported);
			var incTime = Arrays.stream(TIME_TARGET).anyMatch(original::isSupported);
			if (incDate && incTime) {
				return new Timestamp(toEpochMilli(original));
			} else if (incDate) {
				return new Date(toEpochMilli(original));
			} else {
				return new Time(toEpochMilli(original));
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
	 * エポック1970-01-01T00:00:00Zからのミリ秒数 に変換する
	 *
	 * @param temporalAccessor 時間的オブジェクト
	 * @return エポック1970-01-01T00:00:00Zからのミリ秒数
	 */
	private long toEpochMilli(final TemporalAccessor temporalAccessor) {
		var year = getTemporalField(temporalAccessor, ChronoField.YEAR, 1970);
		var month = getTemporalField(temporalAccessor, ChronoField.MONTH_OF_YEAR, 1);
		var dayOfMonth = getTemporalField(temporalAccessor, ChronoField.DAY_OF_MONTH, 1);
		var hour = getTemporalField(temporalAccessor, ChronoField.HOUR_OF_DAY, 0);
		var minute = getTemporalField(temporalAccessor, ChronoField.MINUTE_OF_HOUR, 0);
		var second = getTemporalField(temporalAccessor, ChronoField.SECOND_OF_MINUTE, 0);
		var milliSecond = getTemporalField(temporalAccessor, ChronoField.MILLI_OF_SECOND, 0);
		var nanoOfSecond = getTemporalField(temporalAccessor, ChronoField.NANO_OF_SECOND, milliSecond * 1000_000);
		return ZonedDateTime.of(year, month, dayOfMonth, hour, minute, second, nanoOfSecond,
				clock.getZone()).toInstant().toEpochMilli();
	}

	/**
	 * TemporalFieldの値を取得する
	 *
	 * @param temporalAccessor 時間的オブジェクト
	 * @param chronoField 時間フィールド
	 * @param def 時間的オブジェクトが時間フィールドをサポートしていない場合に取得する値
	 * @return 指定した時間フィールドの値
	 */
	private int getTemporalField(final TemporalAccessor temporalAccessor, final TemporalField chronoField,
			final int def) {
		return temporalAccessor.isSupported(chronoField) ? temporalAccessor.get(chronoField) : def;
	}
}
