package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.text.ParseException;
import java.time.Clock;
import java.time.Month;
import java.time.MonthDay;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class MonthDayToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		var mapper = new MonthDayToStringParameterMapper();

		assertThat(mapper.toJdbc(MonthDay.of(Month.JANUARY, 1), null, null), is("0101"));
		assertThat(mapper.toJdbc(MonthDay.of(Month.DECEMBER, 31), null, null), is("1231"));
	}

	@Test
	public void testManagerToJdbc() throws Exception {
		var manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new MonthDayToStringParameterMapper());

		assertThat(manager.toJdbc(MonthDay.of(Month.JANUARY, 1), null), is("0101"));
	}

}
