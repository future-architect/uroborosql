package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
import java.time.Clock;
import java.time.LocalDate;

import org.junit.Test;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class LocalDateToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		LocalDateToStringParameterMapper mapper = new LocalDateToStringParameterMapper();
		LocalDate localDate = LocalDate.of(2003, 2, 2);

		assertThat(mapper.toJdbc(localDate, null, null), is("20030202"));
	}

	@Test
	public void testManagerToJdbc() throws Exception {
		BindParameterMapperManager manager = new BindParameterMapperManager(Clock.systemDefaultZone());
		manager.addMapper(new LocalDateToStringParameterMapper());

		LocalDate localDate = LocalDate.of(2003, 2, 2);

		assertThat(manager.toJdbc(localDate, null), is("20030202"));
	}

}
