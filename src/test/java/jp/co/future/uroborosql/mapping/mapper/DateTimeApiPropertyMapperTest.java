package jp.co.future.uroborosql.mapping.mapper;

import static jp.co.future.uroborosql.mapping.mapper.Helper.newResultSet;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

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
import java.time.temporal.ChronoUnit;

import jp.co.future.uroborosql.mapping.JavaType;

import org.junit.Test;

public class DateTimeApiPropertyMapperTest {

	@Test
	public void test() throws NoSuchMethodException, SecurityException, SQLException {
		PropertyMapperManager mapper = new PropertyMapperManager();
		LocalDateTime localDateTime = LocalDateTime.now();
		OffsetDateTime offsetDateTime = OffsetDateTime.of(localDateTime, OffsetDateTime.now().getOffset());
		ZonedDateTime zonedDateTime = ZonedDateTime.of(localDateTime, Clock.systemDefaultZone().getZone());

		java.sql.Timestamp timestamp = java.sql.Timestamp.valueOf(localDateTime);
		LocalDate localDate = localDateTime.toLocalDate();
		java.sql.Date date = java.sql.Date.valueOf(localDate);
		LocalTime localTime = localDateTime.toLocalTime().truncatedTo(ChronoUnit.SECONDS);
		java.sql.Time time = java.sql.Time.valueOf(localTime);
		OffsetTime offsetTime = offsetDateTime.toOffsetTime().truncatedTo(ChronoUnit.SECONDS);

		assertThat(mapper.getValue(JavaType.of(LocalDateTime.class), newResultSet("getTimestamp", timestamp), 1), is(localDateTime));
		assertThat(mapper.getValue(JavaType.of(OffsetDateTime.class), newResultSet("getTimestamp", timestamp), 1), is(offsetDateTime));
		assertThat(mapper.getValue(JavaType.of(ZonedDateTime.class), newResultSet("getTimestamp", timestamp), 1), is(zonedDateTime));
		assertThat(mapper.getValue(JavaType.of(LocalDate.class), newResultSet("getDate", date), 1), is(localDate));
		assertThat(mapper.getValue(JavaType.of(LocalTime.class), newResultSet("getTime", time), 1), is(localTime));
		assertThat(mapper.getValue(JavaType.of(OffsetTime.class), newResultSet("getTime", time), 1), is(offsetTime));

		assertThat(mapper.getValue(JavaType.of(LocalDateTime.class), newResultSet("getTimestamp", null), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(OffsetDateTime.class), newResultSet("getTimestamp", null), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(ZonedDateTime.class), newResultSet("getTimestamp", null), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(LocalDate.class), newResultSet("getDate", null), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(LocalTime.class), newResultSet("getTime", null), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(OffsetTime.class), newResultSet("getTime", null), 1), is(nullValue()));

	}

	@Test
	public void test2() throws NoSuchMethodException, SecurityException, SQLException {
		PropertyMapperManager mapper = new PropertyMapperManager();
		assertThat(mapper.getValue(JavaType.of(Year.class), newResultSet("getInt", 2000), 1), is(Year.of(2000)));
		assertThat(mapper.getValue(JavaType.of(YearMonth.class), newResultSet("getInt", 200004), 1), is(YearMonth.of(2000, 4)));
		assertThat(mapper.getValue(JavaType.of(MonthDay.class), newResultSet("getInt", 401), 1), is(MonthDay.of(4, 1)));
		assertThat(mapper.getValue(JavaType.of(Month.class), newResultSet("getInt", 4), 1), is(Month.APRIL));
		assertThat(mapper.getValue(JavaType.of(DayOfWeek.class), newResultSet("getInt", 4), 1), is(DayOfWeek.THURSDAY));

		assertThat(mapper.getValue(JavaType.of(Year.class), newResultSet("getInt", 0, "wasNull", true), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(YearMonth.class), newResultSet("getInt", 0, "wasNull", true), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(MonthDay.class), newResultSet("getInt", 0, "wasNull", true), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(Month.class), newResultSet("getInt", 0, "wasNull", true), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(DayOfWeek.class), newResultSet("getInt", 0, "wasNull", true), 1), is(nullValue()));

	}

	@Test
	public void test3() throws NoSuchMethodException, SecurityException, SQLException {
		PropertyMapperManager mapper = new PropertyMapperManager();

		LocalDate localDate = LocalDate.now();
		java.sql.Date date = java.sql.Date.valueOf(localDate);
		JapaneseDate japaneseDate = JapaneseDate.of(localDate.getYear(), localDate.getMonthValue(), localDate.getDayOfMonth());

		assertThat(mapper.getValue(JavaType.of(JapaneseEra.class), newResultSet("getInt", 2), 1), is(JapaneseEra.HEISEI));
		assertThat(mapper.getValue(JavaType.of(Era.class), newResultSet("getInt", 2), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(JapaneseDate.class), newResultSet("getDate", date), 1), is(japaneseDate));
		assertThat(mapper.getValue(JavaType.of(ChronoLocalDate.class), newResultSet("getDate", null), 1), is(nullValue()));

		assertThat(mapper.getValue(JavaType.of(JapaneseEra.class), newResultSet("getInt", 0, "wasNull", true), 1), is(nullValue()));
		assertThat(mapper.getValue(JavaType.of(JapaneseDate.class), newResultSet("getDate", null), 1), is(nullValue()));

	}
}
