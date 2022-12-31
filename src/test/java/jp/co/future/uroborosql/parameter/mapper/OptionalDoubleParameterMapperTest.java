package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

import java.util.OptionalDouble;

import org.junit.Test;

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
