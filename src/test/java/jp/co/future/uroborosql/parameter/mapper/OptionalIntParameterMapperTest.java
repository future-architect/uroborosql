package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import java.util.OptionalInt;

import org.junit.jupiter.api.Test;

public class OptionalIntParameterMapperTest {

	@Test
	public void test() {
		int value = 123;

		OptionalIntParameterMapper mapper = new OptionalIntParameterMapper();
		OptionalInt optional = OptionalInt.of(value);
		assertThat(mapper.toJdbc(optional, null, null), is(value));
	}

	@Test
	public void testEmpty() {
		OptionalIntParameterMapper mapper = new OptionalIntParameterMapper();
		OptionalInt optional = OptionalInt.empty();
		assertThat(mapper.toJdbc(optional, null, null), is(nullValue()));
	}

}
