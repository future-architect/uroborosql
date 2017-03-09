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
	 *
	 * @param position 開始位置
	 * @param expression 評価式
	 * @param tokenValue トークン上の値
	 */
	public BindVariableNode(final int position, final String expression, final String tokenValue) {
		super(position, 0, expression, tokenValue);
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
		pass();
	}
}
