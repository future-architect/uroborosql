package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.jupiter.api.Test;

public class BigIntegerParameterMapperTest {

	@Test
	public void test() {
		var mapper = new BigIntegerParameterMapper();
		assertThat(mapper.toJdbc(BigInteger.valueOf(1), null, null), is(BigDecimal.valueOf(1)));
	}

}
