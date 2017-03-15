package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parser.TransformContext;

/**
 * ELSE句を表すノード
 *
 * @author H.Sugimoto
 */
public class ElseNode extends ContainerNode {

	/**
	 * コンストラクタ
	 *
	 * @param position 開始位置
	 * @param length データ長
	 */
	public ElseNode(int position, int length) {
		super(position, length);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.ContainerNode#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		super.accept(transformContext);
		transformContext.setEnabled(true);
	}
}