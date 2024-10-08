package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Time;
import java.text.ParseException;
import java.time.Clock;
import java.time.LocalTime;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class SqlTimeToStringParameterMapperTest {

	@Test
	void test() throws ParseException {
		var mapper = new SqlTimeToStringParameterMapper();
		var time = Time.valueOf(LocalTime.parse("11:22:33"));

		assertThat(mapper.toJdbc(time, null, null), is("112233"));
	}

	@Test
	void testManagerToJdbc() throws Exception {
		var manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new SqlTimeToStringParameterMapper());

		var time = Time.valueOf(LocalTime.parse("11:22:33"));

		assertThat(manager.toJdbc(time, null), is("112233"));
	}

}
