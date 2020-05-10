package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
import java.time.Clock;
import java.time.YearMonth;

import org.junit.Test;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class YearMonthToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		YearMonthToStringParameterMapper mapper = new YearMonthToStringParameterMapper();
		YearMonth yearMonth = YearMonth.of(2020, 4);

		assertThat(mapper.toJdbc(yearMonth, null, null), is("202004"));
	}

	@Test
	public void testManagerToJdbc() throws Exception {
		BindParameterMapperManager manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new YearMonthToStringParameterMapper());

		YearMonth yearMonth = YearMonth.of(2020, 4);

		assertThat(manager.toJdbc(yearMonth, null), is("202004"));
	}

}
