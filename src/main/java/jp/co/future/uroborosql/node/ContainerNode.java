package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parser.TransformContext;

/**
 * コンテナノード
 *
 * @author H.Sugimoto
 */
public class ContainerNode extends AbstractNode {

	/**
	 * コンストラクタ
	 */
	public ContainerNode() {
		// do nothing
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		int size = getChildSize();
		for (int i = 0; i < size; i++) {
			getChild(i).accept(transformContext);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.AbstractNode#passed(java.lang.StringBuilder)
	 */
	@Override
	public void passed(final StringBuilder builder) {
		int childSize = getChildSize();
		for (int i = 0; i < childSize; ++i) {
			getChild(i).passed(builder);
		}
	}
}