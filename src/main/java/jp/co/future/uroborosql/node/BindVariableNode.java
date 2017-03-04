package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parser.TransformContext;

/**
 * バインド変数を表すノード
 *
 * @author H.Sugimoto
 */
public class BindVariableNode extends ExpressionNode {

	/**
	 * コンストラクタ
	 * @param expression 評価式
	 */
	public BindVariableNode(final String expression) {
		super(expression);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		Object value = eval(transformContext);

		transformContext.addSqlPart("?/*").addSqlPart(expression).addSqlPart("*/");
		transformContext.addBindName(expression);
		transformContext.addBindVariable(value);
		state = CoverageState.PASSED;
	}
}
