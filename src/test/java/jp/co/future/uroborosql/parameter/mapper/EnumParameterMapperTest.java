package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import org.junit.Test;

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
