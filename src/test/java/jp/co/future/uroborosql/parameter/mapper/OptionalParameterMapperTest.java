package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.time.Clock;
import java.util.Optional;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class OptionalParameterMapperTest {
	private Clock clock = null;

	@BeforeEach
	public void setUp() {
		this.clock = Clock.systemDefaultZone();
	}

	@Test
	public void test() {
		var parameterMapperManager = new BindParameterMapperManager(this.clock);
		var value = "ABC";

		var mapper = new OptionalParameterMapper();
		Optional<String> optional = Optional.of(value);
		assertThat(mapper.toJdbc(optional, null, parameterMapperManager), is(value));

		Optional<Optional<String>> optional2 = Optional.of(Optional.of(value));
		assertThat(mapper.toJdbc(optional2, null, parameterMapperManager), is(value));
	}

	@Test
	public void testEmpty() {
		var parameterMapperManager = new BindParameterMapperManager(this.clock);
		var mapper = new OptionalParameterMapper();
		Optional<String> optional = Optional.empty();
		assertThat(mapper.toJdbc(optional, null, parameterMapperManager), is(nullValue()));
	}
}
