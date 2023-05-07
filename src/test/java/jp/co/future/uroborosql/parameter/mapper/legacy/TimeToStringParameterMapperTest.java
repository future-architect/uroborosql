package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;

import java.text.ParseException;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.temporal.TemporalAccessor;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class TimeToStringParameterMapperTest {

	@Test
	void testLocalTime() throws ParseException {
		var mapper = new TimeToStringParameterMapper();
		var localTime = LocalTime.of(11, 22, 33);
		assertThat(mapper.toJdbc(localTime, null, null), is("112233"));

		localTime = LocalTime.of(11, 22);
		assertThat(mapper.toJdbc(localTime, null, null), is("112200"));
	}

	@Test
	void testOffsetTime() throws ParseException {
		var mapper = new TimeToStringParameterMapper();
		var localTime = LocalTime.of(11, 22, 33);
		var offset = Clock.systemDefaultZone().getZone().getRules().getOffset(localTime.atDate(LocalDate.now()));
		var offsetTime = OffsetTime.of(localTime, offset);
		assertThat(mapper.toJdbc(offsetTime, null, null), is("112233"));

		offsetTime = OffsetTime.of(LocalTime.of(11, 22), offset);
		assertThat(mapper.toJdbc(offsetTime, null, null), is("112200"));
	}

	@Test
	void testCanAccept() throws Exception {
		var mapper = new TimeToStringParameterMapper();

		assertThat(mapper.canAccept(LocalTime.now()), is(true));
		assertThat(mapper.canAccept(OffsetTime.now()), is(true));
		assertThat(mapper.canAccept(LocalDate.now()), is(false));
		assertThat(mapper.canAccept(LocalDateTime.now()), is(false));
		assertThat(mapper.canAccept(OffsetDateTime.now()), is(false));
	}

	@Test
	void testTargetType() throws Exception {
		var mapper = new TimeToStringParameterMapper();

		assertThat(mapper.targetType(), sameInstance(TemporalAccessor.class));
	}

	@Test
	void testManagerToJdbc() throws Exception {
		var manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new TimeToStringParameterMapper());

		var localTime = LocalTime.of(11, 22, 33);

		assertThat(manager.toJdbc(localTime, null), is("112233"));
	}

}
