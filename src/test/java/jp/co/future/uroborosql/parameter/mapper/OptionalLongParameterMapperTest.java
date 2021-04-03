package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import java.util.OptionalLong;

import org.junit.jupiter.api.Test;

public class OptionalLongParameterMapperTest {

	@Test
	public void test() {
		long value = 123;

		OptionalLongParameterMapper mapper = new OptionalLongParameterMapper();
		OptionalLong optional = OptionalLong.of(value);
		assertThat(mapper.toJdbc(optional, null, null), is(value));
	}

	@Test
	public void testEmpty() {
		OptionalLongParameterMapper mapper = new OptionalLongParameterMapper();
		OptionalLong optional = OptionalLong.empty();
		assertThat(mapper.toJdbc(optional, null, null), is(nullValue()));
	}

}
