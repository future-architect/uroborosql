package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.text.ParseException;
import java.time.Clock;
import java.time.Year;

import org.junit.Test;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class YearToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		var mapper = new YearToStringParameterMapper();
		var year = Year.of(2020);

		assertThat(mapper.toJdbc(year, null, null), is("2020"));
	}

	@Test
	public void testManagerToJdbc() throws Exception {
		var manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new YearToStringParameterMapper());

		var year = Year.of(2020);

		assertThat(manager.toJdbc(year, null), is("2020"));
	}

}
