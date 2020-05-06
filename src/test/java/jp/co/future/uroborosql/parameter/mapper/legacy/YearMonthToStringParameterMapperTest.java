package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
import java.time.YearMonth;

import org.junit.Test;

public class YearMonthToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		YearMonthToStringParameterMapper mapper = new YearMonthToStringParameterMapper();
		YearMonth yearMonth = YearMonth.of(2020, 4);

		assertThat(mapper.toJdbc(yearMonth, null, null), is("202004"));
	}
}
