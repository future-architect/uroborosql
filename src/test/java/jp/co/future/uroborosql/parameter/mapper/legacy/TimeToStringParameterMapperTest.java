package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.ZoneOffset;

import org.junit.Test;

public class TimeToStringParameterMapperTest {

	@Test
	public void testLocalTime() throws ParseException {
		TimeToStringParameterMapper mapper = new TimeToStringParameterMapper();
		LocalTime localTime = LocalTime.of(11, 22, 33);
		assertThat(mapper.toJdbc(localTime, null, null), is("112233"));

		localTime = LocalTime.of(11, 22);
		assertThat(mapper.toJdbc(localTime, null, null), is("112200"));
	}

	@Test
	public void testOffsetTime() throws ParseException {
		TimeToStringParameterMapper mapper = new TimeToStringParameterMapper();
		LocalTime localTime = LocalTime.of(11, 22, 33);
		ZoneOffset offset = Clock.systemDefaultZone().getZone().getRules().getOffset(localTime.atDate(LocalDate.now()));
		OffsetTime offsetTime = OffsetTime.of(localTime, offset);
		assertThat(mapper.toJdbc(offsetTime, null, null), is("112233"));

		offsetTime = OffsetTime.of(LocalTime.of(11, 22), offset);
		assertThat(mapper.toJdbc(offsetTime, null, null), is("112200"));
	}

	@Test
	public void testCanAccept() throws Exception {
		TimeToStringParameterMapper mapper = new TimeToStringParameterMapper();

		assertThat(mapper.canAccept(LocalTime.now()), is(true));
		assertThat(mapper.canAccept(OffsetTime.now()), is(true));
		assertThat(mapper.canAccept(LocalDate.now()), is(false));
		assertThat(mapper.canAccept(LocalDateTime.now()), is(false));
		assertThat(mapper.canAccept(OffsetDateTime.now()), is(false));
	}
}
