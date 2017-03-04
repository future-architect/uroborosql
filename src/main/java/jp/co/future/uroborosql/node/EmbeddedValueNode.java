package jp.co.future.uroborosql.node;

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
	 * @param expression 評価式
	 */
	public EmbeddedValueNode(final String expression) {
		this(expression, false);
	}

	/**
	 * コンストラクタ
	 *
	 * @param expression 評価式
	 * @param wrap シングルクォートで囲むかどうか
	 */
	public EmbeddedValueNode(final String expression, final boolean wrap) {
		super(expression);
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
				transformContext.addSqlPart("'").addSqlPart(escapeSql(value)).addSqlPart("'/*#")
						.addSqlPart(expression).addSqlPart("*/");
			} else {
				transformContext.addSqlPart(escapeSql(value)).addSqlPart("/*$").addSqlPart(expression)
						.addSqlPart("*/");
			}
		} else {
			transformContext.addSqlPart(null);
		}
		state = CoverageState.PASSED;
	}

	/**
	 * 値の中の"'"を"''"に置換します
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
