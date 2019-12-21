package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.time.Clock;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;

public class OptionalParameterMapperTest {
	private Clock clock = null;

	@Before
	public void setUp() {
		this.clock = Clock.systemDefaultZone();
	}

	@Test
	public void test() {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager(this.clock);
		String value = "ABC";

		OptionalParameterMapper mapper = new OptionalParameterMapper();
		Optional<String> optional = Optional.of(value);
		assertThat(mapper.toJdbc(optional, null, parameterMapperManager), is(value));

		Optional<Optional<String>> optional2 = Optional.of(Optional.of(value));
		assertThat(mapper.toJdbc(optional2, null, parameterMapperManager), is(value));
	}

	@Test
	public void testEmpty() {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager(this.clock);
		OptionalParameterMapper mapper = new OptionalParameterMapper();
		Optional<String> optional = Optional.empty();
		assertThat(mapper.toJdbc(optional, null, parameterMapperManager), is(nullValue()));
	}
}
