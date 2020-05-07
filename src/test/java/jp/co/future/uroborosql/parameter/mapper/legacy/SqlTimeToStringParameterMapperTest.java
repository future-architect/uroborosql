package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.sql.Time;
import java.text.ParseException;
import java.time.Clock;
import java.time.LocalTime;

import org.junit.Test;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class SqlTimeToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		SqlTimeToStringParameterMapper mapper = new SqlTimeToStringParameterMapper();
		Time time = Time.valueOf(LocalTime.parse("11:22:33"));

		assertThat(mapper.toJdbc(time, null, null), is("112233"));
	}

	@Test
	public void testManagerToJdbc() throws Exception {
		BindParameterMapperManager manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new SqlTimeToStringParameterMapper());

		Time time = Time.valueOf(LocalTime.parse("11:22:33"));

		assertThat(manager.toJdbc(time, null), is("112233"));
	}

}
