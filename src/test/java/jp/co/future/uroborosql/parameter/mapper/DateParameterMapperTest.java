package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.text.ParseException;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

import org.junit.jupiter.api.Test;

public class DateParameterMapperTest {

	@Test
	void test() throws ParseException {
		var mapper = new DateParameterMapper();
		var date = Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());

		assertThat(mapper.toJdbc(date, null, null), instanceOf(java.sql.Timestamp.class));

		assertThat(mapper.toJdbc(date, null, null), is(new java.sql.Timestamp(date.getTime())));
	}

	@Test
	void testSqlDate() throws ParseException {
		var mapper = new DateParameterMapper();
		var date = Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		var sqlDate = new java.sql.Date(date.getTime());
		assertThat(mapper.toJdbc(sqlDate, null, null), instanceOf(java.sql.Date.class));
		assertThat(mapper.toJdbc(sqlDate, null, null), is(sqlDate));

		var timestamp = new java.sql.Timestamp(date.getTime());
		assertThat(mapper.toJdbc(timestamp, null, null), instanceOf(java.sql.Timestamp.class));
		assertThat(mapper.toJdbc(timestamp, null, null), is(timestamp));

		var time = new java.sql.Time(date.getTime());
		assertThat(mapper.toJdbc(time, null, null), instanceOf(java.sql.Time.class));
		assertThat(mapper.toJdbc(time, null, null), is(time));
	}
}
