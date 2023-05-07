package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.jupiter.api.Test;

public class BigIntegerParameterMapperTest {

	@Test
	void test() {
		var mapper = new BigIntegerParameterMapper();
		assertThat(mapper.toJdbc(BigInteger.valueOf(1), null, null), is(BigDecimal.valueOf(1)));
	}

}
