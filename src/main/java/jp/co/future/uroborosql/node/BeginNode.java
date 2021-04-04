/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.coverage.PassedRoute;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * PL/SQLのBEGIN句を表すノード
 *
 * @author H.Sugimoto
 */
public class BeginNode extends BranchNode {

	/**
	 * コンストラクタ
	 *
	 * @param position 開始位置
	 */
	public BeginNode(final int position) {
		super(position, 9);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.ContainerNode#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		var childCtx = transformContext.copyTransformContext();
		super.accept(childCtx);
		if (childCtx.isEnabled()) {
			transformContext.addSqlPart(childCtx.getExecutableSql());
			transformContext.addBindNames(childCtx.getBindNames());
			transformContext.addBindVariables(childCtx.getBindVariables());
			passState(true);
		} else {
			passState(false);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#passed(PassedRoute)
	 */
	@Override
	public void passed(final PassedRoute passed) {
		passed.appendBranchState(getPosition(), getPosition() + getLength() - 1, this.getState());
		super.passed(passed);
	}

}