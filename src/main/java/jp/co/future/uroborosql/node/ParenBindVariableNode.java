package jp.co.future.uroborosql.node;

import java.lang.reflect.Array;
import java.util.List;

import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * カッコつきバインド変数を表すノード （IN句で使用）
 *
 * @author H.Sugimoto
 */
public class ParenBindVariableNode extends ExpressionNode {

	public ParenBindVariableNode(final int position, final String expression, final String tokenValue) {
		super(position, 0, expression, tokenValue);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	@SuppressWarnings({ "rawtypes" })
	public void accept(final TransformContext transformContext) {
		Object var = eval(transformContext);

		if (var == null) {
			throw new ParameterNotFoundRuntimeException("Parameter is not set. [" + expression + "]");
		} else if (var instanceof List) {
			bindArray(transformContext, ((List) var).toArray());
		} else if (var.getClass().isArray()) {
			bindArray(transformContext, var);
		} else {
			bindArray(transformContext, new Object[] { var });
		}
		pass();
	}

	/**
	 * 配列の値をIN句として加工してバインドする
	 *
	 * @param transformContext transformコンテキスト
	 * @param values バインドする値の配列
	 */
	private void bindArray(final TransformContext transformContext, final Object values) {
		int length = Array.getLength(values);
		if (length == 0) {
			throw new ParameterNotFoundRuntimeException("Parameter is not set. [" + expression + "]");
		}

		transformContext.addSqlPart("(?");
		transformContext.addBindVariable(Array.get(values, 0));
		for (int i = 1; i < length; i++) {
			transformContext.addSqlPart(", ?");
			transformContext.addBindVariable(Array.get(values, i));
		}
		transformContext.addSqlPart(")/*").addSqlPart(expression).addSqlPart("*/");
		transformContext.addBindName(expression);
	}
}