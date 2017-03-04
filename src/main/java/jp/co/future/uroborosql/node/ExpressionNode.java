package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.exception.OgnlRuntimeException;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parser.TransformContext;
import ognl.Ognl;
import ognl.OgnlException;

import org.apache.commons.lang3.StringUtils;

/**
 * 値の評価を行うノードの親クラス
 *
 * @author H.Sugimoto
 */
public abstract class ExpressionNode extends AbstractNode {
	/** 評価式 */
	protected final String expression;

	/**
	 * 評価を行うノード
	 *
	 * @param expression 評価式
	 */
	protected ExpressionNode(final String expression) {
		this.expression = expression;
	}

	/**
	 * OGNL式オブジェクトを取得する
	 *
	 * @param expression 評価式
	 * @return OGNL式オブジェクト
	 */
	protected Object getParsedExpression(final String expression) {
		try {
			return Ognl.parseExpression(expression);
		} catch (OgnlException ex) {
			throw new OgnlRuntimeException("式の解析に失敗しました。[" + expression + "]", ex);
		}
	}

	/**
	 * OGNL式として扱うかどうかを判定する
	 *
	 * @param expression 評価式
	 * @return OGNL式として扱う場合<code>true</code>
	 */
	protected boolean isOgnl(final String expression) {
		// 中かっこ（）がある場合はメソッドとして扱う（paramName.methodName(args...))
		return StringUtils.isNotEmpty(expression) && expression.contains("(") && expression.contains(")");
	}

	/**
	 * 値を取得する
	 *
	 * @param transformContext 変換用コンテキスト
	 * @return 取得した値
	 */
	protected Object eval(final TransformContext transformContext) {
		Object value = null;
		if (isOgnl(expression)) {
			// OGNL式の場合はEvalした結果を取得
			// OGNL式の評価は処理が重いため、必要な段階になってからParsedExpressionを取得する
			try {
				value = Ognl.getValue(getParsedExpression(expression), transformContext);
			} catch (OgnlException ex) {
				throw new OgnlRuntimeException("オブジェクトの取得に失敗しました。[" + expression + "]", ex);
			}
		} else {
			// キーの指定の場合は高速化のため、直接DaoContextから取得
			Parameter parameter = transformContext.getParam(expression);
			if (parameter != null) {
				value = parameter.getValue();
			}
		}
		return value;
	}

	/**
	 * 評価式の取得
	 * @return 評価式
	 */
	public final String getExpression() {
		return expression;
	}
}