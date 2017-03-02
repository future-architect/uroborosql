package jp.co.future.uroborosql.parameter.mapper;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Connection;

/**
 * {@link BigInteger}用{@link BindParameterMapper}
 *
 * @author ota
 */
public class BigIntegerParameterMapper implements BindParameterMapper<BigInteger> {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#targetType()
	 */
	@Override
	public Class<BigInteger> targetType() {
		return BigInteger.class;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.mapper.BindParameterMapper#toJdbc(java.lang.Object, java.sql.Connection, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public Object toJdbc(final BigInteger original, final Connection connection,
			final BindParameterMapperManager parameterMapperManager) {
		return new BigDecimal(original);
	}
}
