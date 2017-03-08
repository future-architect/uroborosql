package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.coverage.PassedRoute;
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
	 */
	public ElseNode(int position) {
		super(position);
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

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.ContainerNode#passed(PassedRoute)
	 */
	@Override
	public void passed(final PassedRoute passed) {
		if (isPassed()) {
			passed.appendHitRange(getPosition(), getPosition() + 3);
		}
		super.passed(passed);
	}
}