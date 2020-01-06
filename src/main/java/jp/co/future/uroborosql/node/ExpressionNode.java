/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.expr.Expression;
import jp.co.future.uroborosql.expr.ExpressionParser;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * 値の評価を行うノードの親クラス
 *
 * @author H.Sugimoto
 */
public abstract class ExpressionNode extends AbstractNode {
	protected final ExpressionParser expressionParser;

	/** 評価式 */
	protected final String expression;

	/** トークン上の値 */
	private final String tokenValue;

	/**
	 * 評価を行うノード
	 *
	 * @param expressionParser ExpressionParser
	 * @param position 開始位置
	 * @param addLength 追加データ長
	 * @param expression 評価式
	 * @param tokenValue トークン上の値
	 */
	protected ExpressionNode(final ExpressionParser expressionParser, final int position, final int addLength,
			final String expression, final String tokenValue) {
		super(position, 2 + addLength + expression.length() + 2 + (tokenValue != null ? tokenValue.length() : 0));
		this.expressionParser = expressionParser;
		this.expression = expression;
		this.tokenValue = tokenValue;
	}

	/**
	 * 評価式オブジェクトを取得する
	 *
	 * @param expression 評価式
	 * @return 評価式オブジェクト
	 */
	protected Expression getParsedExpression(final String expression) {
		return expressionParser.parse(expression);
	}

	/**
	 * 評価式として扱うかどうかを判定する
	 *
	 * @param expression 評価式
	 * @return 評価式として扱う場合<code>true</code>
	 */
	protected boolean isPropertyAccess(final String expression) {
		return expressionParser.isPropertyAccess(expression);
	}

	/**
	 * 値を取得する
	 *
	 * @param transformContext 変換用コンテキスト
	 * @return 取得した値
	 */
	protected Object eval(final TransformContext transformContext) {
		Object value = null;
		if (isPropertyAccess(expression)) {
			// キーの指定の場合は高速化のため、直接TransformContextから取得
			Parameter parameter = transformContext.getParam(expression);
			if (parameter != null) {
				value = parameter.getValue();
			}
		} else {
			// 評価式を評価した結果を取得
			// 評価式の評価は処理が重いため、必要な段階になってからParsedExpressionを取得する
			value = getParsedExpression(expression).getValue(transformContext);
			// 評価式の場合は評価した値がバインドパラメータに登録されていないのでこのタイミングで登録する
			transformContext.param(expression, value);
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