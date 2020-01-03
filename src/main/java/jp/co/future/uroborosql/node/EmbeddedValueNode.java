/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.expr.ExpressionParser;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * 埋め込み文字を表すノード
 *
 * <pre>
 * #expression のように#を付けて記述すると、評価の結果を文字列として評価されるようシングルクォートで囲んで出力する。
 * $expression のように$を付けて記述すると、評価の結果をそのまま出力する。
 * </pre>
 *
 * @author H.Sugimoto
 */
public class EmbeddedValueNode extends ExpressionNode {
	/** シングルクォートで囲むかどうか */
	private final boolean wrap;

	/**
	 * コンストラクタ
	 *
	 * @param expressionParser ExpressionParser
	 * @param position 開始位置
	 * @param expression 評価式
	 * @param tokenValue トークン上の値
	 */
	public EmbeddedValueNode(final ExpressionParser expressionParser, final int position, final String expression,
			final String tokenValue) {
		this(expressionParser, position, expression, false, tokenValue);
	}

	/**
	 * コンストラクタ
	 *
	 * @param expressionParser ExpressionParser
	 * @param position 開始位置
	 * @param expression 評価式
	 * @param wrap シングルクォートで囲むかどうか
	 * @param tokenValue トークン上の値
	 */
	public EmbeddedValueNode(final ExpressionParser expressionParser, final int position, final String expression,
			final boolean wrap, final String tokenValue) {
		super(expressionParser, position, 1, expression, tokenValue);
		this.wrap = wrap;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		Object value = eval(transformContext);

		if (value != null) {
			if (wrap) {
				transformContext.addSqlPart("'").addSqlPart(escapeSql(value)).addSqlPart("'/*#").addSqlPart(expression)
						.addSqlPart("*/");
			} else {
				transformContext.addSqlPart(escapeSql(value)).addSqlPart("/*$").addSqlPart(expression).addSqlPart("*/");
			}
		} else {
			transformContext.addSqlPart(null);
		}
		pass();
	}

	/**
	 * 値の中の"'"を"''"に置換します
	 *
	 * @param obj 値
	 * @return 値を文字列に変換後、"'"を"''"に置換した文字列
	 */
	private String escapeSql(final Object obj) {
		if (obj == null) {
			return "";
		} else {
			String val = obj.toString();
			return val.replace("'", "''");
		}
	}
}
