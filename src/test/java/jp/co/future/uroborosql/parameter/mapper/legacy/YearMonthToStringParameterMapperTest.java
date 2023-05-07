package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.text.ParseException;
import java.time.Clock;
import java.time.YearMonth;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class YearMonthToStringParameterMapperTest {

	@Test
	void test() throws ParseException {
		var mapper = new YearMonthToStringParameterMapper();
		var yearMonth = YearMonth.of(2020, 4);

		assertThat(mapper.toJdbc(yearMonth, null, null), is("202004"));
	}

	@Test
	void testManagerToJdbc() throws Exception {
		var manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new YearMonthToStringParameterMapper());

		var yearMonth = YearMonth.of(2020, 4);

		assertThat(manager.toJdbc(yearMonth, null), is("202004"));
	}

}
