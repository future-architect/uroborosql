package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Time;
import java.sql.Timestamp;
import java.text.ParseException;
import java.time.Clock;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

import org.junit.jupiter.api.Test;

public class DateToStringParameterMapperTest {

	@Test
	void test() throws ParseException {
		var mapper = new DateToStringParameterMapper();
		var clock = Clock.systemDefaultZone();
		mapper.setClock(clock);
		assertThat(mapper.getClock(), is(clock));

		var date = Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());

		assertThat(mapper.toJdbc(date, null, null), is("20000101"));
	}

	@Test
	void testSqlDate() throws ParseException {
		var mapper = new DateToStringParameterMapper();
		mapper.setClock(Clock.systemDefaultZone());
		var date = Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		var sqlDate = new java.sql.Date(date.getTime());
		assertThat(mapper.toJdbc(sqlDate, null, null), is("20000101"));
	}

	@Test
	void testCanAccept() throws Exception {
		var mapper = new DateToStringParameterMapper();
		mapper.setClock(Clock.systemDefaultZone());

		assertThat(mapper.canAccept(new Date()), is(true));
		assertThat(mapper.canAccept(new java.sql.Date(new Date().getTime())), is(true));
		assertThat(mapper.canAccept(new Time(new Date().getTime())), is(false));
		assertThat(mapper.canAccept(new Timestamp(new Date().getTime())), is(false));
		assertThat(mapper.canAccept("str"), is(false));
	}

	@Test
	void testTargetType() throws Exception {
		var mapper = new DateToStringParameterMapper();
		mapper.setClock(Clock.systemDefaultZone());

		assertThat(mapper.targetType(), sameInstance(Date.class));
	}

	@Test
	void testManagerToJdbc() throws Exception {
		var manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new DateToStringParameterMapper());

		var date = Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		assertThat(manager.toJdbc(date, null), is("20000101"));
	}

}
