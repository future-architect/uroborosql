package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;

import java.text.ParseException;

import org.junit.Test;

public class EmptyStringToNullParameterMapperTest {

	@Test
	public void test() throws ParseException {
		var mapper = new EmptyStringToNullParameterMapper();

		assertThat(mapper.toJdbc("", null, null), is(nullValue()));
		assertThat(mapper.toJdbc("str", null, null), is("str"));
	}

	@Test
	public void testCanAccept() throws Exception {
		var mapper = new EmptyStringToNullParameterMapper();

		assertThat(mapper.canAccept(""), is(true));
		assertThat(mapper.canAccept("str"), is(false));
		assertThat(mapper.canAccept(1), is(false));
	}

	@Test
	public void testTargetType() throws Exception {
		var mapper = new EmptyStringToNullParameterMapper();

		assertThat(mapper.targetType(), sameInstance(String.class));
	}
}
