package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.OptionalLong;

import org.junit.jupiter.api.Test;

public class OptionalLongParameterMapperTest {

	@Test
	void test() {
		var value = 123L;

		var mapper = new OptionalLongParameterMapper();
		var optional = OptionalLong.of(value);
		assertThat(mapper.toJdbc(optional, null, null), is(value));
	}

	@Test
	void testEmpty() {
		var mapper = new OptionalLongParameterMapper();
		var optional = OptionalLong.empty();
		assertThat(mapper.toJdbc(optional, null, null), is(nullValue()));
	}

}
