package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parser.TransformContext;

/**
 * PL/SQLのBEGIN句を表すノード
 *
 * @author H.Sugimoto
 */
public class BeginNode extends ContainerNode {

	/**
	 * コンストラクタ
	 */
	public BeginNode() {
		// do nothing
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.ContainerNode#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		TransformContext childCtx = transformContext.copyTransformContext();
		super.accept(childCtx);
		if (childCtx.isEnabled()) {
			transformContext.addSqlPart(childCtx.getExecutableSql());
			transformContext.addBindNames(childCtx.getBindNames());
			transformContext.addBindVariables(childCtx.getBindVariables());
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#passed(java.lang.StringBuilder)
	 */
	@Override
	public void passed(final StringBuilder builder) {
		builder.append(state);
		super.passed(builder);
	}

}