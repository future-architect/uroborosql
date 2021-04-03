package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import java.util.OptionalDouble;

import org.junit.jupiter.api.Test;

public class OptionalDoubleParameterMapperTest {

	@Test
	public void test() {
		double value = 12.3;

		OptionalDoubleParameterMapper mapper = new OptionalDoubleParameterMapper();
		OptionalDouble optional = OptionalDouble.of(value);
		assertThat(mapper.toJdbc(optional, null, null), is(value));
	}

	@Test
	public void testEmpty() {
		OptionalDoubleParameterMapper mapper = new OptionalDoubleParameterMapper();
		OptionalDouble optional = OptionalDouble.empty();
		assertThat(mapper.toJdbc(optional, null, null), is(nullValue()));
	}

}
