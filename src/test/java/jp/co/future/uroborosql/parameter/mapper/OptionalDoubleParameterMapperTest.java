package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.OptionalDouble;

import org.junit.jupiter.api.Test;

public class OptionalDoubleParameterMapperTest {

	@Test
	void test() {
		var value = 12.3;

		var mapper = new OptionalDoubleParameterMapper();
		var optional = OptionalDouble.of(value);
		assertThat(mapper.toJdbc(optional, null, null), is(value));
	}

	@Test
	void testEmpty() {
		var mapper = new OptionalDoubleParameterMapper();
		var optional = OptionalDouble.empty();
		assertThat(mapper.toJdbc(optional, null, null), is(nullValue()));
	}

}
