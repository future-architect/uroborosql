package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.util.Optional;

import org.junit.Test;

public class OptionalParameterMapperTest {

	@Test
	public void test() {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager();
		String value = "ABC";

		OptionalParameterMapper mapper = new OptionalParameterMapper();
		Optional<String> optional = Optional.of(value);
		assertThat(mapper.toJdbc(optional, null, parameterMapperManager), is(value));

		Optional<Optional<String>> optional2 = Optional.of(Optional.of(value));
		assertThat(mapper.toJdbc(optional2, null, parameterMapperManager), is(value));
	}

	@Test
	public void testEmpty() {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager();
		OptionalParameterMapper mapper = new OptionalParameterMapper();
		Optional<String> optional = Optional.empty();
		assertThat(mapper.toJdbc(optional, null, parameterMapperManager), is(nullValue()));
	}
}
