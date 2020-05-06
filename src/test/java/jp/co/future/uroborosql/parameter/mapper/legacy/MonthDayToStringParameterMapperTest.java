package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
import java.time.Month;
import java.time.MonthDay;

import org.junit.Test;

public class MonthDayToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		MonthDayToStringParameterMapper mapper = new MonthDayToStringParameterMapper();

		assertThat(mapper.toJdbc(MonthDay.of(Month.JANUARY, 1), null, null), is("0101"));
		assertThat(mapper.toJdbc(MonthDay.of(Month.DECEMBER, 31), null, null), is("1231"));
	}
}
