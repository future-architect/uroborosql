/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.node;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.coverage.PassedRoute;
import jp.co.future.uroborosql.exception.IllegalBoolExpressionRuntimeException;
import jp.co.future.uroborosql.exception.OgnlRuntimeException;
import jp.co.future.uroborosql.parser.TransformContext;
import jp.co.future.uroborosql.utils.StringFunction;
import ognl.ASTProperty;
import ognl.Node;
import ognl.Ognl;
import ognl.OgnlException;

/**
 * IF句を表すノード
 *
 * @author H.Sugimoto
 */
public class IfNode extends BranchNode {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(IfNode.class);

	/** 評価式 */
	private final String expression;

	/** ELSE句 */
	private ElseNode elseNode;

	/** ELSEIF句 */
	private IfNode elseIfNode;

	/**
	 * コンストラクタ
	 *
	 * @param position 開始位置
	 * @param expression 評価式
	 */
	public IfNode(final int position, final String expression) {
		super(position, expression.length() + 6);
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
		Object result = null;
		Node parsedExpression = null;
		try {
			parsedExpression = (Node) Ognl.parseExpression(expression);
			result = Ognl.getValue(parsedExpression, transformContext, null);
		} catch (OgnlException ex) {
			throw new OgnlRuntimeException("Value could not be obtained.[" + expression + "]", ex);
		}

		if (result instanceof Boolean) {
			boolean resultValue = ((Boolean) result).booleanValue();
			if (LOG.isDebugEnabled()) {
				if (Boolean.TRUE.toString().equalsIgnoreCase(expression)
						|| Boolean.FALSE.toString().equalsIgnoreCase(expression)) {
					// 単純なBoolean評価の場合はログを出力しない
				} else {
					StringBuilder builder = new StringBuilder();
					dumpNode(parsedExpression, transformContext, builder);

					LOG.debug("Evaluation Expression:[{}], Result:[{}], Parameter:[{}]", expression, resultValue,
							builder.length() == 0 ? ""
									: builder.substring(0, builder.length() - 1));
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

	/**
	 * ExpressionのNodeを解析してパラメータの値をダンプします
	 *
	 * @param node expressionの各ノード
	 * @param transformContext
	 * @param builder パラメータの値を出力する先
	 */
	private void dumpNode(final Node node, final TransformContext transformContext, final StringBuilder builder) {
		if (node == null) {
			return;
		}
		if (node instanceof ASTProperty) {
			ASTProperty prop = (ASTProperty) node;
			if (!StringFunction.SHORT_NAME.equals(prop.toString())) {
				try {
					Object value = Ognl.getValue(prop, transformContext, null);
					builder.append(prop)
							.append(":[")
							.append(value == null ? null : value.toString())
							.append("],");
				} catch (OgnlException ex) {
					// ダンプ処理でシステムが止まっては困るのでスタックトレースを出して握りつぶす
					ex.printStackTrace();
				}
			}
		} else {
			int childCount = node.jjtGetNumChildren();
			for (int i = 0; i < childCount; i++) {
				Node child = node.jjtGetChild(i);
				dumpNode(child, transformContext, builder);
			}
		}
	}

}
