package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.sql.Time;
import java.sql.Timestamp;
import java.text.ParseException;
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
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.chrono.JapaneseDate;
import java.time.chrono.JapaneseEra;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalField;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class DateTimeApiParameterMapperTest {
	private Clock clock = null;

	@BeforeEach
	public void setUp() {
		this.clock = Clock.systemDefaultZone();
	}

	@Test
	public void testLocalDateTime() throws ParseException {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(LocalDateTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(Timestamp.class));
		assertThat(mapper.toJdbc(LocalDateTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createTimestamp("2000-01-01T10:10:10")));
	}

	@Test
	public void testOffsetDateTime() throws ParseException {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(OffsetDateTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(Timestamp.class));
		assertThat(
				mapper.toJdbc(
						OffsetDateTime.from(createDateTime("2000-01-01T10:10:10")).withOffsetSameInstant(
								ZoneOffset.ofHours(0)),
						null, null),
				is(createTimestamp("2000-01-01T10:10:10")));
	}

	@Test
	public void testZonedDateTime() throws ParseException {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(createDateTime("2000-01-01T10:10:10"), null, null),
				instanceOf(Timestamp.class));
		assertThat(mapper.toJdbc(
				createDateTime("2000-01-01T10:10:10").withZoneSameInstant(ZoneId.of("Europe/Monaco")), null,
				null),
				is(createTimestamp("2000-01-01T10:10:10")));
	}

	@Test
	public void testLocalDate() throws ParseException {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(LocalDate.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(java.sql.Date.class));
		assertThat(mapper.toJdbc(LocalDate.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createDate("2000-01-01")));
	}

	@Test
	public void testLocalTime() throws ParseException {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(LocalTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(Time.class));
		assertThat(mapper.toJdbc(LocalTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createTime("10:10:10")));
	}

	@Test
	public void testOffsetTime() throws ParseException {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(OffsetTime.from(createDateTime("2000-01-01T10:10:10.123")), null, null),
				instanceOf(Time.class));
		assertThat(mapper.toJdbc(OffsetTime.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createTime("10:10:10")));
	}

	@Test
	public void testYear() {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(Year.of(2000), null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(Year.of(2000), null, null), is(2000));
		assertThat(mapper.toJdbc(Year.of(200), null, null), is(200));
	}

	@Test
	public void testYearMonth() {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(YearMonth.of(2001, Month.JANUARY), null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(YearMonth.of(2001, Month.JANUARY), null, null), is(200101));
		assertThat(mapper.toJdbc(YearMonth.of(201, Month.JANUARY), null, null), is(20101));
	}

	@Test
	public void testMonthDay() {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(MonthDay.of(Month.JANUARY, 10), null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(MonthDay.of(Month.JANUARY, 10), null, null), is(110));
		assertThat(mapper.toJdbc(MonthDay.of(Month.JANUARY, 1), null, null), is(101));
	}

	@Test
	public void testMonth() {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(Month.JANUARY, null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(Month.JANUARY, null, null), is(1));
		assertThat(mapper.toJdbc(Month.DECEMBER, null, null), is(12));
	}

	@Test
	public void testDayOfWeek() {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(DayOfWeek.SUNDAY, null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(DayOfWeek.SUNDAY, null, null), is(7));
		assertThat(mapper.toJdbc(DayOfWeek.MONDAY, null, null), is(1));
	}

	@Test
	public void testEra() {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(JapaneseEra.HEISEI, null, null), instanceOf(Integer.class));
		assertThat(mapper.toJdbc(JapaneseEra.HEISEI, null, null), is(2));
		assertThat(mapper.toJdbc(JapaneseEra.MEIJI, null, null), is(-1));
	}

	@Test
	public void testChronoLocalDate() throws ParseException {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(JapaneseDate.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(java.sql.Date.class));
		assertThat(mapper.toJdbc(JapaneseDate.from(createDateTime("2000-01-01T10:10:10")), null, null),
				is(createDate("2000-01-01")));
	}

	@Test
	public void testInstant() throws ParseException {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(Instant.from(createDateTime("2000-01-01T10:10:10")), null, null),
				instanceOf(Timestamp.class));
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

		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(temporalAccessor, null, null), instanceOf(java.sql.Date.class));
		assertThat(mapper.toJdbc(temporalAccessor, null, null), is(createDate("2000-01-05")));
	}

	@Test
	public void testTemporalAccessorDateTime1() throws ParseException {
		TemporalAccessor temporalAccessor = new TemporalAccessor() {

			@Override
			public boolean isSupported(final TemporalField field) {
				return field == ChronoField.YEAR || field == ChronoField.DAY_OF_MONTH
						|| field == ChronoField.MILLI_OF_SECOND;
			}

			@Override
			public long getLong(final TemporalField field) {
				if (field == ChronoField.YEAR) {
					return 2000;
				} else if (field == ChronoField.DAY_OF_MONTH) {
					return 5;
				} else if (field == ChronoField.MILLI_OF_SECOND) {
					return 123;
				}
				return field.getFrom(this);
			}
		};

		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(temporalAccessor, null, null), instanceOf(Timestamp.class));
		assertThat(mapper.toJdbc(temporalAccessor, null, null),
				is(new Timestamp(createDateTime("2000-01-05T00:00:00.123").toInstant().toEpochMilli())));
	}

	@Test
	public void testTemporalAccessorDate2() throws ParseException {
		var temporalAccessor = DateTimeFormatter.ofPattern("yyyy-MM-dd HH").parse("2000-05-05 10");

		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(temporalAccessor, null, null), instanceOf(Timestamp.class));
		assertThat(mapper.toJdbc(temporalAccessor, null, null), is(createTimestamp("2000-05-05T10:00:00")));
	}

	@Test
	public void testTemporalAccessorDate3() throws ParseException {
		var temporalAccessor = DateTimeFormatter.ofPattern("HH").parse("10");

		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(temporalAccessor, null, null), instanceOf(Time.class));
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

		var mapper = new DateTimeApiParameterMapper(this.clock);
		assertThat(mapper.toJdbc(temporalAccessor, null, null), instanceOf(TemporalAccessor.class));
		assertThat(mapper.toJdbc(temporalAccessor, null, null), is(temporalAccessor));
	}

	@Test
	public void testClock() {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		mapper.setClock(this.clock);
		assertThat(mapper.getClock(), is(this.clock));
	}

	public void testTargetType() {
		var mapper = new DateTimeApiParameterMapper(this.clock);
		mapper.setClock(this.clock);
		assertThat(mapper.targetType(), sameInstance(TemporalAccessor.class));
	}

	private Timestamp createTimestamp(final String s) throws ParseException {
		return Timestamp.valueOf(LocalDateTime.parse(s));
	}

	private Time createTime(final String s) throws ParseException {
		return Time.valueOf(LocalTime.parse(s, DateTimeFormatter.ISO_LOCAL_TIME));
	}

	private java.sql.Date createDate(final String s) throws ParseException {
		return java.sql.Date.valueOf(LocalDate.parse(s));
	}

	private ZonedDateTime createDateTime(final String s) throws ParseException {
		return ZonedDateTime.parse(s + "+09:00[Asia/Tokyo]");
	}

}
