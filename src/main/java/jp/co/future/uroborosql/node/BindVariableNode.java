package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parser.TransformContext;

/**
 * バインド変数を表すノード
 *
 * @author H.Sugimoto
 */
public class BindVariableNode extends ExpressionNode {
	/** バインド変数置換後にバインド変数のコメント文字列を出力するかどうか */
	private final boolean outputBindComment;

	/**
	 * コンストラクタ
	 *
	 * @param position 開始位置
	 * @param expression 評価式
	 * @param tokenValue トークン上の値
	 * @param outputBindComment バインド変数置換後にバインド変数のコメント文字列を出力するかどうか
	 */
	public BindVariableNode(final int position, final String expression, final String tokenValue,
			final boolean outputBindComment) {
		super(position, 0, expression, tokenValue);
		this.outputBindComment = outputBindComment;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		Object value = eval(transformContext);

		transformContext.addSqlPart("?");
		if (outputBindComment) {
			transformContext.addSqlPart("/*").addSqlPart(expression).addSqlPart("*/");
		}
		transformContext.addBindName(expression);
		transformContext.addBindVariable(value);
		pass();
	}
}
