package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

public class EnumParameterMapperTest {
	enum TestEnum {
		A, B
	}

	@Test
	void test() {
		var mapper = new EnumParameterMapper();
		assertThat(mapper.toJdbc(TestEnum.A, null, null), is("A"));
	}

}
