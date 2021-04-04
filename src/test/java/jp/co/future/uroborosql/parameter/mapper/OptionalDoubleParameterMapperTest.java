package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.OptionalDouble;

import org.junit.jupiter.api.Test;

public class OptionalDoubleParameterMapperTest {

	@Test
	public void test() {
		var value = 12.3;

		var mapper = new OptionalDoubleParameterMapper();
		var optional = OptionalDouble.of(value);
		assertThat(mapper.toJdbc(optional, null, null), is(value));
	}

	@Test
	public void testEmpty() {
		var mapper = new OptionalDoubleParameterMapper();
		var optional = OptionalDouble.empty();
		assertThat(mapper.toJdbc(optional, null, null), is(nullValue()));
	}

}
