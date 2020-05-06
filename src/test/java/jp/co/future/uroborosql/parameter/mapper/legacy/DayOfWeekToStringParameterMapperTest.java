package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
import java.time.DayOfWeek;

import org.junit.Test;

public class DayOfWeekToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		DayOfWeekToStringParameterMapper mapper = new DayOfWeekToStringParameterMapper();

		assertThat(mapper.toJdbc(DayOfWeek.SUNDAY, null, null), is("7"));
		assertThat(mapper.toJdbc(DayOfWeek.FRIDAY, null, null), is("5"));
	}
}
