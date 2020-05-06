package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;

import org.junit.Test;

public class DateTimeToStringParameterMapperTest {

	@Test
	public void testLocalDateTime() throws ParseException {
		DateTimeToStringParameterMapper mapper = new DateTimeToStringParameterMapper();
		LocalDateTime dateTime = LocalDateTime.of(2020, 01, 02, 11, 22, 33, 123000000);

		assertThat(mapper.toJdbc(dateTime, null, null), is("20200102112233123"));
	}

	@Test
	public void testOffsetDateTime() throws ParseException {
		DateTimeToStringParameterMapper mapper = new DateTimeToStringParameterMapper();
		LocalDateTime localDateTime = LocalDateTime.of(2020, 01, 02, 11, 22, 33, 123000000);
		ZoneOffset offset = Clock.systemDefaultZone().getZone().getRules().getOffset(localDateTime);
		OffsetDateTime dateTime = OffsetDateTime.of(localDateTime, offset);

		assertThat(mapper.toJdbc(dateTime, null, null), is("20200102112233123"));
	}

	@Test
	public void testZonedDateTime() throws ParseException {
		DateTimeToStringParameterMapper mapper = new DateTimeToStringParameterMapper();
		LocalDateTime localDateTime = LocalDateTime.of(2020, 01, 02, 11, 22, 33, 123000000);
		ZonedDateTime dateTime = ZonedDateTime.of(localDateTime, Clock.systemDefaultZone().getZone());

		assertThat(mapper.toJdbc(dateTime, null, null), is("20200102112233123"));
	}

	@Test
	public void testCanAccept() throws Exception {
		DateTimeToStringParameterMapper mapper = new DateTimeToStringParameterMapper();

		assertThat(mapper.canAccept(LocalDateTime.now()), is(true));
		assertThat(mapper.canAccept(OffsetDateTime.now()), is(true));
		assertThat(mapper.canAccept(ZonedDateTime.now()), is(true));
		assertThat(mapper.canAccept(LocalDate.now()), is(false));
		assertThat(mapper.canAccept(LocalTime.now()), is(false));
	}
}
