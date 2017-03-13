package jp.co.future.uroborosql.node;

import org.apache.commons.lang3.StringUtils;

import jp.co.future.uroborosql.exception.OgnlRuntimeException;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parser.TransformContext;
import ognl.Ognl;
import ognl.OgnlException;

/**
 * 値の評価を行うノードの親クラス
 *
 * @author H.Sugimoto
 */
public abstract class ExpressionNode extends AbstractNode {
	/** 評価式 */
	protected final String expression;

	/** トークン上の値 */
	private final String tokenValue;

	/**
	 * 評価を行うノード
	 *
	 * @param position 開始位置
	 * @param addLength 追加データ長
	 * @param expression 評価式
	 * @param tokenValue トークン上の値
	 */
	protected ExpressionNode(final int position, final int addLength, final String expression,
			final String tokenValue) {
		super(position, 2 + addLength + expression.length() + 2 + (tokenValue != null ? tokenValue.length() : 0));
		this.expression = expression;
		this.tokenValue = tokenValue;
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
	 *
	 * @return 評価式
	 */
	public final String getExpression() {
		return expression;
	}

	/**
	 * トークン上の値の取得
	 *
	 * @return トークン上の値
	 */
	public String getTokenValue() {
		return tokenValue;
	}
}