package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.text.ParseException;
import java.time.Clock;
import java.time.DayOfWeek;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

import org.junit.jupiter.api.Test;

public class DayOfWeekToStringParameterMapperTest {

	@Test
	void test() throws ParseException {
		var mapper = new DayOfWeekToStringParameterMapper();

		assertThat(mapper.toJdbc(DayOfWeek.SUNDAY, null, null), is("7"));
		assertThat(mapper.toJdbc(DayOfWeek.FRIDAY, null, null), is("5"));
	}

	@Test
	void testManagerToJdbc() throws Exception {
		var manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new DayOfWeekToStringParameterMapper());

		assertThat(manager.toJdbc(DayOfWeek.SUNDAY, null), is("7"));
	}

}
