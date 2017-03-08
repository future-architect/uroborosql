package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.coverage.PassedRoute;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * コンテナノード
 *
 * @author H.Sugimoto
 */
public class ContainerNode extends AbstractNode {

	/**
	 * コンストラクタ
	 *
	 * @param position 開始位置
	 */
	public ContainerNode(int position) {
		super(position);
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
	 * @see jp.co.future.uroborosql.node.AbstractNode#passed(PassedRoute)
	 */
	@Override
	public void passed(final PassedRoute passed) {
		int childSize = getChildSize();
		for (int i = 0; i < childSize; ++i) {
			getChild(i).passed(passed);
		}
	}
}