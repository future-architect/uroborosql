package jp.co.future.uroborosql.node;

import java.lang.reflect.Array;
import java.util.List;

import jp.co.future.uroborosql.exception.OgnlRuntimeException;
import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;
import jp.co.future.uroborosql.parser.TransformContext;
import ognl.Ognl;
import ognl.OgnlException;

/**
 * カッコつきバインド変数を表すノード
 * （IN句で使用）
 *
 * @author H.Sugimoto
 */
public class ParenBindVariableNode extends AbstractNode {

	/** 評価式 */
	private final String expression;

	/** 解析ずみ評価式 */
	private Object parsedExpression;

	public ParenBindVariableNode(final String expression) {
		this.expression = expression;
		try {
			parsedExpression = Ognl.parseExpression(expression);
		} catch (OgnlException ex) {
			throw new OgnlRuntimeException("コメントの解析に失敗しました。[" + expression + "]", ex);
		}
	}

	/**
	 * 評価式の取得
	 *
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
	@SuppressWarnings({ "rawtypes" })
	public void accept(final TransformContext transformContext) {
		Object var = null;
		try {
			var = Ognl.getValue(parsedExpression, transformContext);
		} catch (OgnlException ex) {
			throw new OgnlRuntimeException("値が取得できませんでした。[" + expression + "]", ex);
		}

		if (var == null) {
			throw new ParameterNotFoundRuntimeException("パラメータが設定されていません。[" + expression + "]");
		} else if (var instanceof List) {
			bindArray(transformContext, ((List) var).toArray());
		} else if (var.getClass().isArray()) {
			bindArray(transformContext, var);
		} else {
			bindArray(transformContext, new Object[] { var });
		}
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
			throw new ParameterNotFoundRuntimeException("パラメータが設定されていません。[" + expression + "]");
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