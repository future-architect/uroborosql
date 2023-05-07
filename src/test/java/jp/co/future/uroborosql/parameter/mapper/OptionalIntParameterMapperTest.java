package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.OptionalInt;

import org.junit.jupiter.api.Test;

public class OptionalIntParameterMapperTest {

	@Test
	void test() {
		var value = 123;

		var mapper = new OptionalIntParameterMapper();
		var optional = OptionalInt.of(value);
		assertThat(mapper.toJdbc(optional, null, null), is(value));
	}

	@Test
	void testEmpty() {
		var mapper = new OptionalIntParameterMapper();
		var optional = OptionalInt.empty();
		assertThat(mapper.toJdbc(optional, null, null), is(nullValue()));
	}

}
