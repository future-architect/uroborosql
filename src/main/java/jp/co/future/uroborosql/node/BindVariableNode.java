package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * バインド変数を表すノード
 *
 * @author H.Sugimoto
 */
public class BindVariableNode extends AbstractNode {

	/** 評価式 */
	private final String expression;

	/**
	 * コンストラクタ
	 * @param expression 評価式
	 */
	public BindVariableNode(final String expression) {
		this.expression = expression;
	}

	/**
	 * 評価式の取得
	 * @return 評価式
	 */
	public String getExpression() {
		return expression;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		Parameter parameter = transformContext.getParam(expression);
		Object value = null;
		if (parameter != null) {
			value = parameter.getValue();
		}

		transformContext.addSqlPart("?/*").addSqlPart(expression).addSqlPart("*/");
		transformContext.addBindName(expression);
		transformContext.addBindVariable(value);
	}
}
