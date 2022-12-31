package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;

public class EnumParameterMapperTest {
	enum TestEnum {
		A, B
	}

	@Test
	public void test() {
		var mapper = new EnumParameterMapper();
		assertThat(mapper.toJdbc(TestEnum.A, null, null), is("A"));
	}

}
