/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.expr.ognl;

import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

import jp.co.future.uroborosql.exception.ExpressionRuntimeException;
import jp.co.future.uroborosql.expr.AbstractExpressionParser;
import jp.co.future.uroborosql.expr.Expression;
import jp.co.future.uroborosql.log.support.ParserLoggingSupport;
import jp.co.future.uroborosql.parser.TransformContext;
import jp.co.future.uroborosql.utils.SqlFunction;
import ognl.ASTProperty;
import ognl.Node;
import ognl.Ognl;
import ognl.OgnlException;
import ognl.OgnlRuntime;

/**
 * OGNLを使用した評価式パーサー
 *
 * @author H.Sugimoto
 */
public class OgnlExpressionParser extends AbstractExpressionParser {
	/**
	 * コンストラクタ
	 */
	public OgnlExpressionParser() {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.ExpressionParser#initialize()
	 */
	@Override
	public void initialize() {
		super.initialize();
		OgnlRuntime.setPropertyAccessor(TransformContext.class,
				new TransformContextPropertyAccessor(getSqlConfig().getDialect().getSqlFunction()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.ExpressionParser#parse(java.lang.String)
	 */
	@Override
	public Expression parse(final String expression) {
		try {
			return new OgnlExpression(Ognl.parseExpression(expression));
		} catch (OgnlException ex) {
			throw new ExpressionRuntimeException("Failed to parse the expression.[" + expression + "]", ex);
		}
	}

	/**
	 * OGNL評価式
	 *
	 * @author H.Sugimoto
	 */
	private static class OgnlExpression implements Expression, ParserLoggingSupport {
		private final Object expression;

		/**
		 * コンストラクタ
		 *
		 * @param expression 評価式
		 */
		public OgnlExpression(final Object expression) {
			this.expression = expression;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.expr.Expression#getValue(java.lang.Object)
		 */
		@Override
		public Object getValue(final Object context) {
			try {
				return Ognl.getValue(expression, context);
			} catch (OgnlException ex) {
				throw new ExpressionRuntimeException("Acquire an object failed.[" + expression + "]", ex);
			}
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.expr.Expression#dumpNode(java.lang.Object)
		 */
		@Override
		public StringBuilder dumpNode(final Object context) {
			var builder = new StringBuilder();
			if (expression != null) {
				Set<ASTProperty> props = new LinkedHashSet<>();
				traverseNode((Node) expression, props);
				for (var prop : props) {
					var propName = prop.toString();
					if (!SqlFunction.SHORT_NAME.equals(propName)) {
						try {
							var value = Ognl.getValue(prop, context, null);
							builder.append(propName)
									.append(":[")
									.append(Objects.toString(value, null))
									.append("],");
						} catch (OgnlException ex) {
							// ダンプ処理でシステムが止まっては困るのでログ出力して握りつぶす
							warnWith(PARSER_LOG)
									.setMessage(ex.getMessage())
									.setCause(ex)
									.log();
						}
					}
				}
			}
			return builder;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.expr.Expression#collectParams(java.util.Set)
		 */
		@Override
		public void collectParams(final Set<String> params) {
			Set<ASTProperty> props = new LinkedHashSet<>();
			traverseNode((Node) expression, props);
			for (var prop : props) {
				var propName = prop.toString();
				if (!SqlFunction.SHORT_NAME.equals(propName)) {
					params.add(propName);
				}
			}
		}

		/**
		 * 評価式の探索
		 *
		 * @param node ノード
		 * @param params プロパティが見つかった場合に格納するSetオブジェクト
		 */
		private void traverseNode(final Node node, final Set<ASTProperty> props) {
			if (node == null) {
				return;
			}
			if (node instanceof ASTProperty) {
				var prop = (ASTProperty) node;
				props.add(prop);
			} else {
				var childCount = node.jjtGetNumChildren();
				for (var i = 0; i < childCount; i++) {
					var child = node.jjtGetChild(i);
					traverseNode(child, props);
				}
			}
		}
	}
}
