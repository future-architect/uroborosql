package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.OptionalLong;

import org.junit.jupiter.api.Test;

public class OptionalLongParameterMapperTest {

	@Test
	public void test() {
		var value = 123L;

		var mapper = new OptionalLongParameterMapper();
		var optional = OptionalLong.of(value);
		assertThat(mapper.toJdbc(optional, null, null), is(value));
	}

	@Test
	public void testEmpty() {
		var mapper = new OptionalLongParameterMapper();
		var optional = OptionalLong.empty();
		assertThat(mapper.toJdbc(optional, null, null), is(nullValue()));
	}

}
