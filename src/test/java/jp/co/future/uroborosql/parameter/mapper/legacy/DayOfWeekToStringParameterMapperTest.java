package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.text.ParseException;
import java.time.Clock;
import java.time.DayOfWeek;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class DayOfWeekToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		DayOfWeekToStringParameterMapper mapper = new DayOfWeekToStringParameterMapper();

		assertThat(mapper.toJdbc(DayOfWeek.SUNDAY, null, null), is("7"));
		assertThat(mapper.toJdbc(DayOfWeek.FRIDAY, null, null), is("5"));
	}

	@Test
	public void testManagerToJdbc() throws Exception {
		BindParameterMapperManager manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new DayOfWeekToStringParameterMapper());

		assertThat(manager.toJdbc(DayOfWeek.SUNDAY, null), is("7"));
	}

}
