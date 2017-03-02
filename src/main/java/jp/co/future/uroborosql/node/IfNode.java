package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.exception.IllegalBoolExpressionRuntimeException;
import jp.co.future.uroborosql.exception.OgnlRuntimeException;
import jp.co.future.uroborosql.parser.TransformContext;
import ognl.ASTProperty;
import ognl.ExpressionSyntaxException;
import ognl.Node;
import ognl.Ognl;
import ognl.OgnlException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * IF句を表すノード
 *
 * @author H.Sugimoto
 */
public class IfNode extends ContainerNode {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(IfNode.class);

	/** 評価式 */
	private final String expression;

	/** 解析ずみ評価式 */
	private Object parsedExpression;

	/** ELSE句 */
	private ElseNode elseNode;

	/** ELSEIF句 */
	private IfNode elseIfNode;

	/**
	 * コンストラクタ
	 *
	 * @param expression 評価式
	 */
	public IfNode(final String expression) {
		this.expression = expression;
		try {
			parsedExpression = Ognl.parseExpression(expression);
		} catch (OgnlException e) {
			throw new OgnlRuntimeException("コメントの解析に失敗しました。[" + expression + "]", e);
		}
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
		try {
			result = Ognl.getValue(parsedExpression, transformContext);
		} catch (ExpressionSyntaxException ex) {
			throw new OgnlRuntimeException("コメントの解析に失敗しました。[" + expression + "]", ex);
		} catch (OgnlException ex) {
			throw new OgnlRuntimeException("値が取得できませんでした。[" + expression + "]", ex);
		}

		if (result instanceof Boolean) {
			boolean resultValue = ((Boolean) result).booleanValue();
			if (LOG.isDebugEnabled()) {
				if (Boolean.TRUE.toString().equalsIgnoreCase(expression)
						|| Boolean.FALSE.toString().equalsIgnoreCase(expression)) {
					// 単純なBoolean評価の場合はログを出力しない
				} else {
					Node node = (Node) parsedExpression;
					StringBuilder builder = new StringBuilder();
					dumpNode(node, transformContext, builder);

					LOG.debug("評価式：[{}], 判定結果：[{}], パラメータ：[{}]", expression, resultValue, builder.length() == 0 ? ""
							: builder.substring(0, builder.length() - 1));
				}
			}
			if (resultValue) {
				super.accept(transformContext);
				transformContext.setEnabled(true);
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
			try {
				builder.append(prop).append(":[").append(Ognl.getValue(prop, transformContext)).append("],");
			} catch (OgnlException ex) {
				// ダンプ処理でシステムが止まっては困るのでスタックトレースを出して握りつぶす
				ex.printStackTrace();
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
