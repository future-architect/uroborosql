package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.text.ParseException;
import java.time.Clock;
import java.time.LocalDate;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

import org.junit.jupiter.api.Test;

public class LocalDateToStringParameterMapperTest {

	@Test
	void test() throws ParseException {
		var mapper = new LocalDateToStringParameterMapper();
		var localDate = LocalDate.of(2003, 2, 2);

		assertThat(mapper.toJdbc(localDate, null, null), is("20030202"));
	}

	@Test
	void testManagerToJdbc() throws Exception {
		var manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new LocalDateToStringParameterMapper());

		var localDate = LocalDate.of(2003, 2, 2);

		assertThat(manager.toJdbc(localDate, null), is("20030202"));
	}

}
