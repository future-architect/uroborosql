package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import org.junit.jupiter.api.Test;

public class EnumParameterMapperTest {
	enum TestEnum {
		A, B
	}

	@Test
	public void test() {
		EnumParameterMapper mapper = new EnumParameterMapper();
		assertThat(mapper.toJdbc(TestEnum.A, null, null), is("A"));
	}

}
