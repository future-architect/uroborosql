package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.sql.Time;
import java.text.ParseException;
import java.time.LocalTime;

import org.junit.Test;

public class SqlTimeToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		SqlTimeToStringParameterMapper mapper = new SqlTimeToStringParameterMapper();
		Time time = Time.valueOf(LocalTime.parse("11:22:33"));

		assertThat(mapper.toJdbc(time, null, null), is("112233"));
	}
}
