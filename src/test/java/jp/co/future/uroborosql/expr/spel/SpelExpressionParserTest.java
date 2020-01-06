package jp.co.future.uroborosql.expr.spel;

import jp.co.future.uroborosql.expr.AbstractExpressionParserTest;
import jp.co.future.uroborosql.expr.ExpressionParser;

public class SpelExpressionParserTest extends AbstractExpressionParserTest {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.AbstractExpressionParserTest#getExpressionParser()
	 */
	@Override
	protected ExpressionParser getExpressionParser() {
		return new SpelExpressionParser();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.AbstractExpressionParserTest#getPerformanceHeader()
	 */
	@Override
	protected String getPerformanceHeader() {
		return "SpEL";
	}

}
