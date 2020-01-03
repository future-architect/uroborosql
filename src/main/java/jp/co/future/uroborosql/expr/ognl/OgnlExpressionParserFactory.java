package jp.co.future.uroborosql.expr.ognl;

import jp.co.future.uroborosql.expr.AbstractExpressionParserFactory;
import jp.co.future.uroborosql.expr.ExpressionParser;

/**
 * OGNLを利用した評価式パーサーのファクトリクラス
 *
 * @author H.Sugimoto
 */
public class OgnlExpressionParserFactory extends AbstractExpressionParserFactory {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.ExpressionParserFactory#accept()
	 */
	@Override
	public boolean accept() {
		return existsTargetClass("ognl.OgnlRuntime");
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.ExpressionParserFactory#create()
	 */
	@Override
	public ExpressionParser create() {
		return createExpressionParser("jp.co.future.uroborosql.expr.ognl.OgnlExpressionParser");
	}
}
