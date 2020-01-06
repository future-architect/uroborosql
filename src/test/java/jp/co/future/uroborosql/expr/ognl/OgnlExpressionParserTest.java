package jp.co.future.uroborosql.expr.ognl;

import jp.co.future.uroborosql.expr.AbstractExpressionParserTest;
import jp.co.future.uroborosql.expr.ExpressionParser;

public class OgnlExpressionParserTest extends AbstractExpressionParserTest {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.AbstractExpressionParserTest#getExpressionParser()
	 */
	@Override
	protected ExpressionParser getExpressionParser() {
		return new OgnlExpressionParser();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.AbstractExpressionParserTest#getPerformanceHeader()
	 */
	@Override
	protected String getPerformanceHeader() {
		return "OGNL";
	}

}
