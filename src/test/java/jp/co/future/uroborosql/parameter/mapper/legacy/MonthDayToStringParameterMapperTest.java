package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.text.ParseException;
import java.time.Clock;
import java.time.Month;
import java.time.MonthDay;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

import org.junit.jupiter.api.Test;

public class MonthDayToStringParameterMapperTest {

	@Test
	void test() throws ParseException {
		var mapper = new MonthDayToStringParameterMapper();

		assertThat(mapper.toJdbc(MonthDay.of(Month.JANUARY, 1), null, null), is("0101"));
		assertThat(mapper.toJdbc(MonthDay.of(Month.DECEMBER, 31), null, null), is("1231"));
	}

	@Test
	void testManagerToJdbc() throws Exception {
		var manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new MonthDayToStringParameterMapper());

		assertThat(manager.toJdbc(MonthDay.of(Month.JANUARY, 1), null), is("0101"));
	}

}
