package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.text.ParseException;

import org.junit.Test;

public class EmptyStringToNullParameterMapperTest {

	@Test
	public void test() throws ParseException {
		EmptyStringToNullParameterMapper mapper = new EmptyStringToNullParameterMapper();

		assertThat(mapper.toJdbc("", null, null), is(nullValue()));
		assertThat(mapper.toJdbc("str", null, null), is("str"));
	}

	@Test
	public void testCanAccept() throws Exception {
		EmptyStringToNullParameterMapper mapper = new EmptyStringToNullParameterMapper();

		assertThat(mapper.canAccept(""), is(true));
		assertThat(mapper.canAccept("str"), is(false));
		assertThat(mapper.canAccept(1), is(false));
	}

	@Test
	public void testTargetType() throws Exception {
		EmptyStringToNullParameterMapper mapper = new EmptyStringToNullParameterMapper();

		assertThat(mapper.targetType(), sameInstance(String.class));
	}
}
