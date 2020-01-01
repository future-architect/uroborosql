package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
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
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.chrono.JapaneseDate;
import java.time.chrono.JapaneseEra;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalField;

import org.junit.Test;

public class DateTimeApiParameterMapperTest {
	@Test
	public void testLocalDateTime() throws ParseException {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(LocalDateTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(java.sql.Timestamp.class));
		assertThat(mapper.toJdbc(LocalDateTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createTimestamp("2000-01-01T10:10:10")));
	}

	@Test
	public void testOffsetDateTime() throws ParseException {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(OffsetDateTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(java.sql.Timestamp.class));
		assertThat(
				mapper.toJdbc(
						OffsetDateTime.from(createDateTime("2000-01-01T10:10:10")).withOffsetSameInstant(
								ZoneOffset.ofHours(0)),
						null, null),
				is(createTimestamp("2000-01-01T10:10:10")));
	}

	@Test
	public void testZonedDateTime() throws ParseException {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(createDateTime("2000-01-01T10:10:10"), null, null),
				instanceOf(java.sql.Timestamp.class));
		assertThat(mapper.toJdbc(
				createDateTime("2000-01-01T10:10:10").withZoneSameInstant(ZoneId.of("Europe/Monaco")), null,
				null),
				is(createTimestamp("2000-01-01T10:10:10")));
	}

	@Test
	public void testLocalDate() throws ParseException {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(LocalDate.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(java.sql.Date.class));
		assertThat(mapper.toJdbc(LocalDate.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createDate("2000-01-01")));
	}

	@Test
	public void testLocalTime() throws ParseException {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(LocalTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(java.sql.Time.class));
		assertThat(mapper.toJdbc(LocalTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createTime("10:10:10")));
	}

	@Test
	public void testOffsetTime() throws ParseException {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(OffsetTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(java.sql.Time.class));
		assertThat(mapper.toJdbc(OffsetTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createTime("10:10:10")));
	}

	@Test
	public void testYear() {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(Year.of(2000), null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(Year.of(2000), null, null), is(2000));
		assertThat(mapper.toJdbc(Year.of(200), null, null), is(200));
	}

	@Test
	public void testYearMonth() {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(YearMonth.of(2001, Month.JANUARY), null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(YearMonth.of(2001, Month.JANUARY), null, null), is(200101));
		assertThat(mapper.toJdbc(YearMonth.of(201, Month.JANUARY), null, null), is(20101));
	}

	@Test
	public void testMonthDay() {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(MonthDay.of(Month.JANUARY, 10), null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(MonthDay.of(Month.JANUARY, 10), null, null), is(110));
		assertThat(mapper.toJdbc(MonthDay.of(Month.JANUARY, 1), null, null), is(101));
	}

	@Test
	public void testMonth() {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(Month.JANUARY, null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(Month.JANUARY, null, null), is(1));
		assertThat(mapper.toJdbc(Month.DECEMBER, null, null), is(12));
	}

	@Test
	public void testDayOfWeek() {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(DayOfWeek.SUNDAY, null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(DayOfWeek.SUNDAY, null, null), is(7));
		assertThat(mapper.toJdbc(DayOfWeek.MONDAY, null, null), is(1));
	}

	@Test
	public void testEra() {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(JapaneseEra.HEISEI, null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(JapaneseEra.HEISEI, null, null), is(2));
		assertThat(mapper.toJdbc(JapaneseEra.MEIJI, null, null), is(-1));
	}

	@Test
	public void testChronoLocalDate() throws ParseException {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(JapaneseDate.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(java.sql.Date.class));
		assertThat(mapper.toJdbc(JapaneseDate.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createDate("2000-01-01")));
	}

	@Test
	public void testInstant() throws ParseException {
		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(Instant.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(java.sql.Timestamp.class));
		assertThat(mapper.toJdbc(Instant.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createTimestamp("2000-01-01T10:10:10")));
	}

	@Test
	public void testTemporalAccessorDate1() throws ParseException {
		TemporalAccessor temporalAccessor = new TemporalAccessor() {

			@Override
			public boolean isSupported(final TemporalField field) {
				return field == ChronoField.YEAR || field == ChronoField.DAY_OF_MONTH;
			}

			@Override
			public long getLong(final TemporalField field) {
				if (field == ChronoField.YEAR) {
					return 2000;
				} else if (field == ChronoField.DAY_OF_MONTH) {
					return 5;
				}
				return field.getFrom(this);
			}
		};

		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(temporalAccessor, null, null), instanceOf(java.sql.Date.class));
		assertThat(mapper.toJdbc(temporalAccessor, null, null), is(createDate("2000-01-05")));
	}

	@Test
	public void testTemporalAccessorDate2() throws ParseException {
		TemporalAccessor temporalAccessor = DateTimeFormatter.ofPattern("yyyy-MM-dd HH").parse("2000-05-05 10");

		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(temporalAccessor, null, null), instanceOf(java.sql.Timestamp.class));
		assertThat(mapper.toJdbc(temporalAccessor, null, null), is(createTimestamp("2000-05-05T10:00:00")));
	}

	@Test
	public void testTemporalAccessorDate3() throws ParseException {
		TemporalAccessor temporalAccessor = DateTimeFormatter.ofPattern("HH").parse("10");

		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(temporalAccessor, null, null), instanceOf(java.sql.Time.class));
		assertThat(mapper.toJdbc(temporalAccessor, null, null), is(createTime("10:00:00")));
	}

	@Test
	public void testNonTarget() {
		TemporalAccessor temporalAccessor = new TemporalAccessor() {

			@Override
			public boolean isSupported(final TemporalField field) {
				if (field instanceof ChronoField) {
					return field == ChronoField.ERA || field == ChronoField.YEAR_OF_ERA;
				}
				return field != null && field.isSupportedBy(this);
			}

			@Override
			public long getLong(final TemporalField field) {
				if (field == ChronoField.ERA) {
					return 2;
				} else if (field == ChronoField.YEAR_OF_ERA) {
					return 5;
				}
				return field.getFrom(this);
			}
		};

		DateTimeApiParameterMapper mapper = new DateTimeApiParameterMapper();
		assertThat(mapper.toJdbc(temporalAccessor, null, null), instanceOf(TemporalAccessor.class));
		assertThat(mapper.toJdbc(temporalAccessor, null, null), is(temporalAccessor));
	}

	private java.sql.Timestamp createTimestamp(final String s) throws ParseException {
		return java.sql.Timestamp.valueOf(LocalDateTime.parse(s));
	}

	private java.sql.Time createTime(final String s) throws ParseException {
		return java.sql.Time.valueOf(LocalTime.parse(s, DateTimeFormatter.ofPattern("HH:mm:ss")));
	}

	private java.sql.Date createDate(final String s) throws ParseException {
		return java.sql.Date.valueOf(LocalDate.parse(s));
	}

	private ZonedDateTime createDateTime(final String s) throws ParseException {
		return ZonedDateTime.parse(s + "+09:00[Asia/Tokyo]");
	}

}
