package jp.co.future.uroborosql.expr.spel;

import jp.co.future.uroborosql.expr.AbstractExpressionParserFactory;
import jp.co.future.uroborosql.expr.ExpressionParser;

/**
 * SpringELを利用した評価式パーサーのファクトリクラス
 *
 * @author H.Sugimoto
 */
public class SpelExpressionParserFactory extends AbstractExpressionParserFactory {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.ExpressionParserFactory#accept()
	 */
	@Override
	public boolean accept() {
		return existsTargetClass("org.springframework.expression.ExpressionParser");
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.ExpressionParserFactory#create()
	 */
	@Override
	public ExpressionParser create() {
		return createExpressionParser("jp.co.future.uroborosql.expr.spel.SpelExpressionParser");
	}

}
