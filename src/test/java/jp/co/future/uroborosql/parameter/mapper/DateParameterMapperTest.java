package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
import java.util.Date;

import org.junit.Test;

import jp.co.future.uroborosql.utils.DateUtils;

public class DateParameterMapperTest {

	@Test
	public void test() throws ParseException {
		DateParameterMapper mapper = new DateParameterMapper();
		Date date = DateUtils.parseDate("2000/01/01", new String[] { "yyyy/MM/dd" });
		assertThat(mapper.toJdbc(date, null, null), instanceOf(java.sql.Timestamp.class));

		assertThat(mapper.toJdbc(date, null, null), is(new java.sql.Timestamp(date.getTime())));
	}

	@Test
	public void testSqlDate() throws ParseException {
		DateParameterMapper mapper = new DateParameterMapper();
		Date date = DateUtils.parseDate("2000/01/01", new String[] { "yyyy/MM/dd" });
		java.sql.Date sqlDate = new java.sql.Date(date.getTime());
		assertThat(mapper.toJdbc(sqlDate, null, null), instanceOf(java.sql.Date.class));
		assertThat(mapper.toJdbc(sqlDate, null, null), is(sqlDate));

		java.sql.Timestamp timestamp = new java.sql.Timestamp(date.getTime());
		assertThat(mapper.toJdbc(timestamp, null, null), instanceOf(java.sql.Timestamp.class));
		assertThat(mapper.toJdbc(timestamp, null, null), is(timestamp));

		java.sql.Time time = new java.sql.Time(date.getTime());
		assertThat(mapper.toJdbc(time, null, null), instanceOf(java.sql.Time.class));
		assertThat(mapper.toJdbc(time, null, null), is(time));
	}
}
