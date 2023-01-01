package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;

import java.text.ParseException;

import org.junit.jupiter.api.Test;

public class EmptyStringToNullParameterMapperTest {

	@Test
	void test() throws ParseException {
		var mapper = new EmptyStringToNullParameterMapper();

		assertThat(mapper.toJdbc("", null, null), is(nullValue()));
		assertThat(mapper.toJdbc("str", null, null), is("str"));
	}

	@Test
	void testCanAccept() throws Exception {
		var mapper = new EmptyStringToNullParameterMapper();

		assertThat(mapper.canAccept(""), is(true));
		assertThat(mapper.canAccept("str"), is(false));
		assertThat(mapper.canAccept(1), is(false));
	}

	@Test
	void testTargetType() throws Exception {
		var mapper = new EmptyStringToNullParameterMapper();

		assertThat(mapper.targetType(), sameInstance(String.class));
	}
}
