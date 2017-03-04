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
	 */
	public ElseNode() {
		// do nothing
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
		state = CoverageState.PASSED;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.ContainerNode#passed(java.lang.StringBuilder)
	 */
	@Override
	public void passed(final StringBuilder builder) {
		builder.append(state);
		super.passed(builder);
	}
}