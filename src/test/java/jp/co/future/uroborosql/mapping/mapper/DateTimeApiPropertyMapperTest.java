package jp.co.future.uroborosql.mapping.mapper;

import static jp.co.future.uroborosql.mapping.mapper.Helper.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

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
import java.time.chrono.JapaneseDate;
import java.time.chrono.JapaneseEra;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalField;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.mapping.JavaType;

public class DateTimeApiPropertyMapperTest {
	private Clock clock;

	@BeforeEach
	public void setUp() {
		this.clock = Clock.systemDefaultZone();
	}

	@Test
	public void test() throws NoSuchMethodException, SecurityException, SQLException {
		PropertyMapperManager mapper = new PropertyMapperManager(this.clock);
		LocalDateTime localDateTime = LocalDateTime.now(this.clock).truncatedTo(ChronoUnit.MILLIS);
		OffsetDateTime offsetDateTime = OffsetDateTime.of(localDateTime, OffsetDateTime.now(this.clock).getOffset());
		ZonedDateTime zonedDateTime = ZonedDateTime.of(localDateTime, this.clock.getZone());

		java.sql.Timestamp timestamp = java.sql.Timestamp.valueOf(localDateTime);
		LocalDate localDate = localDateTime.toLocalDate();
		java.sql.Date date = java.sql.Date.valueOf(localDate);
		LocalTime localTime = localDateTime.toLocalTime();
		java.sql.Time time = new java.sql.Time(toEpochMilli(localTime));
		OffsetTime offsetTime = offsetDateTime.toOffsetTime();

		assertThat(mapper.getValue(JavaType.of(LocalDateTime.class), newResultSet("getTimestamp", timestamp), 1),
				is(localDateTime));
		assertThat(mapper.getValue(JavaType.of(LocalDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ofPattern("yyyyMMddHHmmss").format(localDateTime)), 1),
				is(localDateTime.with(ChronoField.MILLI_OF_SECOND, 0L)));
		assertThat(mapper.getValue(JavaType.of(LocalDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSS").format(localDateTime)),
				1), is(localDateTime));
		assertThat(mapper.getValue(JavaType.of(LocalDateTime.class),
				newResultSet("getTimestamp",
						DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(localDateTime)),
				1), is(localDateTime.with(ChronoField.MILLI_OF_SECOND, 0L)));
		assertThat(mapper.getValue(JavaType.of(LocalDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(localDateTime)),
				1), is(localDateTime));
		assertThat(mapper.getValue(JavaType.of(LocalDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(localDateTime)), 1),
				is(localDateTime));

		assertThat(mapper.getValue(JavaType.of(OffsetDateTime.class), newResultSet("getTimestamp", timestamp), 1),
				is(offsetDateTime));
		assertThat(mapper.getValue(JavaType.of(OffsetDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ofPattern("yyyyMMddHHmmss").format(localDateTime)), 1),
				is(offsetDateTime.with(ChronoField.MILLI_OF_SECOND, 0L)));
		assertThat(mapper.getValue(JavaType.of(OffsetDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSS").format(localDateTime)),
				1), is(offsetDateTime));
		assertThat(mapper.getValue(JavaType.of(OffsetDateTime.class),
				newResultSet("getTimestamp",
						DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(localDateTime)),
				1), is(offsetDateTime.with(ChronoField.MILLI_OF_SECOND, 0L)));
		assertThat(mapper.getValue(JavaType.of(OffsetDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(localDateTime)),
				1), is(offsetDateTime));
		assertThat(mapper.getValue(JavaType.of(OffsetDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(localDateTime)), 1),
				is(offsetDateTime));

		assertThat(mapper.getValue(JavaType.of(ZonedDateTime.class), newResultSet("getTimestamp", timestamp), 1),
				is(zonedDateTime));
		assertThat(mapper.getValue(JavaType.of(ZonedDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ofPattern("yyyyMMddHHmmss").format(localDateTime)), 1),
				is(zonedDateTime.with(ChronoField.MILLI_OF_SECOND, 0L)));
		assertThat(mapper.getValue(JavaType.of(ZonedDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSS").format(localDateTime)),
				1), is(zonedDateTime));
		assertThat(mapper.getValue(JavaType.of(ZonedDateTime.class),
				newResultSet("getTimestamp",
						DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(localDateTime)),
				1), is(zonedDateTime.with(ChronoField.MILLI_OF_SECOND, 0L)));
		assertThat(mapper.getValue(JavaType.of(ZonedDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(localDateTime)),
				1), is(zonedDateTime));
		assertThat(mapper.getValue(JavaType.of(ZonedDateTime.class),
				newResultSet("getTimestamp", DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(localDateTime)), 1),
				is(zonedDateTime));

		assertThat(mapper.getValue(JavaType.of(LocalDate.class), newResultSet("getDate", date), 1), is(localDate));
		assertThat(mapper.getValue(JavaType.of(LocalDate.class),
				newResultSet("getDate", DateTimeFormatter.BASIC_ISO_DATE.format(localDate)), 1), is(localDate));
		assertThat(mapper.getValue(JavaType.of(LocalDate.class),
				newResultSet("getDate", DateTimeFormatter.ISO_LOCAL_DATE.format(localDate)), 1), is(localDate));

		assertThat(mapper.getValue(JavaType.of(LocalTime.class), newResultSet("getTime", time), 1), is(localTime));
		assertThat(mapper.getValue(JavaType.of(LocalTime.class),
				newResultSet("getTime", DateTimeFormatter.ofPattern("HHmm").format(localTime)), 1),
				is(localTime.withSecond(0).with(ChronoField.MILLI_OF_SECOND, 0L)));
		assertThat(mapper.getValue(JavaType.of(LocalTime.class),
				newResultSet("getTime", DateTimeFormatter.ofPattern("HHmmss").format(localTime)), 1),
				is(localTime.with(ChronoField.MILLI_OF_SECOND, 0L)));
		assertThat(mapper.getValue(JavaType.of(LocalTime.class),
				newResultSet("getTime", DateTimeFormatter.ofPattern("HHmmssSSS").format(localTime)), 1),
				is(localTime));
		assertThat(mapper.getValue(JavaType.of(LocalTime.class),
				newResultSet("getTime", DateTimeFormatter.ISO_LOCAL_TIME.format(localTime)), 1),
				is(localTime));

		assertThat(mapper.getValue(JavaType.of(OffsetTime.class), newResultSet("getTime", time), 1), is(offsetTime));
		assertThat(mapper.getValue(JavaType.of(OffsetTime.class),
				newResultSet("getTime", DateTimeFormatter.ofPattern("HHmm").format(localTime)), 1),
				is(offsetTime.withSecond(0).with(ChronoField.MILLI_OF_SECOND, 0L)));
		assertThat(mapper.getValue(JavaType.of(OffsetTime.class),
				newResultSet("getTime", DateTimeFormatter.ofPattern("HHmmss").format(localTime)), 1),
				is(offsetTime.with(ChronoField.MILLI_OF_SECOND, 0L)));
		assertThat(mapper.getValue(JavaType.of(OffsetTime.class),
				newResultSet("getTime", DateTimeFormatter.ofPattern("HHmmssSSS").format(localTime)), 1),
				is(offsetTime));
		assertThat(mapper.getValue(JavaType.of(OffsetTime.class),
				newResultSet("getTime", DateTimeFormatter.ISO_LOCAL_TIME.format(localTime)), 1),
				is(offsetTime));

		assertThat(mapper.getValue(JavaType.of(LocalDateTime.class), newResultSet("getTimestamp", null), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(LocalDateTime.class), newResultSet("getTimestamp", ""), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(OffsetDateTime.class), newResultSet("getTimestamp", null), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(OffsetDateTime.class), newResultSet("getTimestamp", ""), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(ZonedDateTime.class), newResultSet("getTimestamp", null), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(ZonedDateTime.class), newResultSet("getTimestamp", ""), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(LocalDate.class), newResultSet("getDate", null), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(LocalDate.class), newResultSet("getDate", ""), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(LocalTime.class), newResultSet("getTime", null), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(LocalTime.class), newResultSet("getTime", ""), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(OffsetTime.class), newResultSet("getTime", null), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(OffsetTime.class), newResultSet("getTime", ""), 1), is(nullValue()));
	}

	@Test
	public void test2() throws NoSuchMethodException, SecurityException, SQLException {
		PropertyMapperManager mapper = new PropertyMapperManager(this.clock);
		assertThat(mapper.getValue(JavaType.of(Year.class), newResultSet("getInt", 2000), 1), is(Year.of(2000)));
		assertThat(mapper.getValue(JavaType.of(Year.class), newResultSet("getInt", "2000"), 1), is(Year.of(2000)));
		assertThat(mapper.getValue(JavaType.of(YearMonth.class), newResultSet("getInt", 200004), 1),
				is(YearMonth.of(2000, 4)));
		assertThat(mapper.getValue(JavaType.of(YearMonth.class), newResultSet("getInt", "200004"), 1),
				is(YearMonth.of(2000, 4)));
		assertThat(mapper.getValue(JavaType.of(MonthDay.class), newResultSet("getInt", 401), 1), is(MonthDay.of(4, 1)));
		assertThat(mapper.getValue(JavaType.of(MonthDay.class), newResultSet("getInt", "401"), 1),
				is(MonthDay.of(4, 1)));
		assertThat(mapper.getValue(JavaType.of(Month.class), newResultSet("getInt", 4), 1), is(Month.APRIL));
		assertThat(mapper.getValue(JavaType.of(Month.class), newResultSet("getInt", "4"), 1), is(Month.APRIL));
		assertThat(mapper.getValue(JavaType.of(DayOfWeek.class), newResultSet("getInt", 4), 1), is(DayOfWeek.THURSDAY));
		assertThat(mapper.getValue(JavaType.of(DayOfWeek.class), newResultSet("getInt", "4"), 1),
				is(DayOfWeek.THURSDAY));

		assertThat(mapper.getValue(JavaType.of(Year.class), newResultSet("getInt", 0, "wasNull", true), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(Year.class), newResultSet("getInt", "", "wasNull", true), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(YearMonth.class), newResultSet("getInt", 0, "wasNull", true), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(YearMonth.class), newResultSet("getInt", "", "wasNull", true), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(MonthDay.class), newResultSet("getInt", 0, "wasNull", true), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(MonthDay.class), newResultSet("getInt", "", "wasNull", true), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(Month.class), newResultSet("getInt", 0, "wasNull", true), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(Month.class), newResultSet("getInt", "", "wasNull", true), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(DayOfWeek.class), newResultSet("getInt", 0, "wasNull", true), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(DayOfWeek.class), newResultSet("getInt", "", "wasNull", true), 1),
				is(nullValue()));
	}

	@Test
	public void test3() throws NoSuchMethodException, SecurityException, SQLException {
		PropertyMapperManager mapper = new PropertyMapperManager(this.clock);

		LocalDate localDate = LocalDate.now(this.clock);
		java.sql.Date date = java.sql.Date.valueOf(localDate);
		JapaneseDate japaneseDate = JapaneseDate.of(localDate.getYear(), localDate.getMonthValue(),
				localDate.getDayOfMonth());

		assertThat(mapper.getValue(JavaType.of(JapaneseEra.class), newResultSet("getInt", 2), 1),
				is(JapaneseEra.HEISEI));
		assertThat(mapper.getValue(JavaType.of(JapaneseEra.class), newResultSet("getInt", "2"), 1),
				is(JapaneseEra.HEISEI));

		assertThat(mapper.getValue(JavaType.of(Era.class), newResultSet("getInt", 2), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(Era.class), newResultSet("getInt", "2"), 1), is(nullValue()));

		assertThat(mapper.getValue(JavaType.of(JapaneseDate.class), newResultSet("getDate", date), 1),
				is(japaneseDate));
		assertThat(mapper.getValue(JavaType.of(JapaneseDate.class),
				newResultSet("getDate", DateTimeFormatter.BASIC_ISO_DATE.format(localDate)), 1),
				is(japaneseDate));

		assertThat(mapper.getValue(JavaType.of(ChronoLocalDate.class), newResultSet("getDate", null), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(ChronoLocalDate.class), newResultSet("getDate", ""), 1),
				is(nullValue()));

		assertThat(mapper.getValue(JavaType.of(JapaneseEra.class), newResultSet("getInt", 0, "wasNull", true), 1),
				is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(JapaneseEra.class), newResultSet("getInt", "", "wasNull", true), 1),
				is(nullValue()));

		assertThat(mapper.getValue(JavaType.of(JapaneseDate.class), newResultSet("getDate", null), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(JapaneseDate.class), newResultSet("getDate", ""), 1), is(nullValue()));

	}

	/**
	 * エポック1970-01-01T00:00:00Zからのミリ秒数 に変換する
	 *
	 * @param temporalAccessor 時間的オブジェクト
	 * @return エポック1970-01-01T00:00:00Zからのミリ秒数
	 */
	private long toEpochMilli(final TemporalAccessor temporalAccessor) {
		int year = getTemporalField(temporalAccessor, ChronoField.YEAR, 1970);
		int month = getTemporalField(temporalAccessor, ChronoField.MONTH_OF_YEAR, 1);
		int dayOfMonth = getTemporalField(temporalAccessor, ChronoField.DAY_OF_MONTH, 1);
		int hour = getTemporalField(temporalAccessor, ChronoField.HOUR_OF_DAY, 0);
		int minute = getTemporalField(temporalAccessor, ChronoField.MINUTE_OF_HOUR, 0);
		int second = getTemporalField(temporalAccessor, ChronoField.SECOND_OF_MINUTE, 0);
		int milliSecond = getTemporalField(temporalAccessor, ChronoField.MILLI_OF_SECOND, 0);
		int nanoOfSecond = getTemporalField(temporalAccessor, ChronoField.NANO_OF_SECOND, milliSecond * 1000_000);
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
