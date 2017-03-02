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
}