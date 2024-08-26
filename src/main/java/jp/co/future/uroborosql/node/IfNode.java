/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.coverage.PassedRoute;
import jp.co.future.uroborosql.exception.IllegalBoolExpressionRuntimeException;
import jp.co.future.uroborosql.expr.ExpressionParser;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * IF句を表すノード
 *
 * @author H.Sugimoto
 */
public class IfNode extends BranchNode {
	private final ExpressionParser expressionParser;
	/** 評価式 */
	private final String expression;

	/** ELSE句 */
	private ElseNode elseNode;

	/** ELSEIF句 */
	private IfNode elseIfNode;

	/**
	 * コンストラクタ
	 *
	 * @param expressionParser ExpressionParser
	 * @param position 開始位置
	 * @param expression 評価式
	 */
	public IfNode(final ExpressionParser expressionParser, final int position, final String expression) {
		super(position, expression.length() + 6);
		this.expressionParser = expressionParser;
		this.expression = expression.trim();
	}

	/**
	 * 評価式の取得
	 *
	 * @return 評価式
	 */
	public String getExpression() {
		return expression;
	}

	/**
	 * ELSE句の取得
	 *
	 * @return ELSE句
	 */
	public ElseNode getElseNode() {
		return elseNode;
	}

	/**
	 * ELSE句の設定
	 *
	 * @param elseNode ELSE句
	 */
	public void setElseNode(final ElseNode elseNode) {
		this.elseNode = elseNode;
	}

	/**
	 * ELSEIF句の取得
	 *
	 * @return ELSEIF句
	 */
	public IfNode getElseIfNode() {
		return elseIfNode;
	}

	/**
	 * ELSEIF句の設定
	 *
	 * @param elseIfNode ELSEIF句
	 */
	public void setElseIfNode(final IfNode elseIfNode) {
		this.elseIfNode = elseIfNode;
	}

	@Override
	public void accept(final TransformContext transformContext) {
		var expr = expressionParser.parse(expression);
		var result = expr.getValue(transformContext);

		if (result instanceof Boolean) {
			var resultValue = (Boolean) result;
			if (PARSER_LOG.isInfoEnabled()) {
				if (Boolean.TRUE.toString().equalsIgnoreCase(expression)
						|| Boolean.FALSE.toString().equalsIgnoreCase(expression)) {
					// 単純なBoolean評価の場合はログを出力しない
				} else {
					PARSER_LOG.atInfo()
							.setMessage("Evaluation Expression:[{}], Result:[{}], Parameter:[{}]")
							.addArgument(expression)
							.addArgument(resultValue)
							.addArgument(() -> {
								var builder = expr.dumpNode(transformContext);
								return builder.length() == 0 ? "" : builder.substring(0, builder.length() - 1);
							})
							.log();
				}
			}
			passState(resultValue);
			if (resultValue) {
				transformContext.setEnabled(true);
				super.accept(transformContext);
			} else if (elseIfNode != null) {
				elseIfNode.accept(transformContext);
			} else if (elseNode != null) {
				elseNode.accept(transformContext);
			}
		} else {
			throw new IllegalBoolExpressionRuntimeException(expression);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.ContainerNode#passed(PassedRoute)
	 */
	@Override
	public void passed(final PassedRoute passed) {
		passed.appendBranchState(getPosition(), getPosition() + getLength() - 1, getState());
		super.passed(passed);
		if (elseIfNode != null) {
			elseIfNode.passed(passed);
		}
		if (elseNode != null) {
			elseNode.passed(passed);
		}
	}
}
