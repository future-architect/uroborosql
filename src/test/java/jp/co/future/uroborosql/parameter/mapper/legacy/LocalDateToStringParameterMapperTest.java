package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;
import java.time.LocalDate;

import org.junit.Test;

public class LocalDateToStringParameterMapperTest {

	@Test
	public void test() throws ParseException {
		LocalDateToStringParameterMapper mapper = new LocalDateToStringParameterMapper();
		LocalDate localDate = LocalDate.of(2003, 2, 2);

		assertThat(mapper.toJdbc(localDate, null, null), is("20030202"));
	}
}
