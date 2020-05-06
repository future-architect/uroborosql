package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
import java.time.Month;

import org.junit.Test;

public class MonthToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		MonthToStringParameterMapper mapper = new MonthToStringParameterMapper();

		assertThat(mapper.toJdbc(Month.APRIL, null, null), is("04"));
		assertThat(mapper.toJdbc(Month.NOVEMBER, null, null), is("11"));
	}
}
