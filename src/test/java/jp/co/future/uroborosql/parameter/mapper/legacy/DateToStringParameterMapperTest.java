package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.sql.Time;
import java.sql.Timestamp;
import java.text.ParseException;
import java.time.Clock;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

import org.junit.Test;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class DateToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		DateToStringParameterMapper mapper = new DateToStringParameterMapper();
		mapper.setClock(Clock.systemDefaultZone());
		Date date = Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());

		assertThat(mapper.toJdbc(date, null, null), is("20000101"));
	}

	@Test
	public void testSqlDate() throws ParseException {
		DateToStringParameterMapper mapper = new DateToStringParameterMapper();
		mapper.setClock(Clock.systemDefaultZone());
		Date date = Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		java.sql.Date sqlDate = new java.sql.Date(date.getTime());
		assertThat(mapper.toJdbc(sqlDate, null, null), is("20000101"));
	}

	@Test
	public void testCanAccept() throws Exception {
		DateToStringParameterMapper mapper = new DateToStringParameterMapper();
		mapper.setClock(Clock.systemDefaultZone());

		assertThat(mapper.canAccept(new Date()), is(true));
		assertThat(mapper.canAccept(new java.sql.Date(new Date().getTime())), is(true));
		assertThat(mapper.canAccept(new Time(new Date().getTime())), is(false));
		assertThat(mapper.canAccept(new Timestamp(new Date().getTime())), is(false));
		assertThat(mapper.canAccept("str"), is(false));
	}

	@Test
	public void testTargetType() throws Exception {
		DateToStringParameterMapper mapper = new DateToStringParameterMapper();
		mapper.setClock(Clock.systemDefaultZone());

		assertThat(mapper.targetType(), sameInstance(Date.class));
	}

	@Test
	public void testManagerToJdbc() throws Exception {
		BindParameterMapperManager manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new DateToStringParameterMapper());

		Date date = Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		assertThat(manager.toJdbc(date, null), is("20000101"));
	}

}
