/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.expr.spel;

import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

import org.springframework.expression.EvaluationException;
import org.springframework.expression.spel.ExpressionState;
import org.springframework.expression.spel.SpelNode;
import org.springframework.expression.spel.ast.PropertyOrFieldReference;
import org.springframework.expression.spel.standard.SpelExpression;
import org.springframework.expression.spel.support.StandardEvaluationContext;

import jp.co.future.uroborosql.exception.ExpressionRuntimeException;
import jp.co.future.uroborosql.expr.AbstractExpressionParser;
import jp.co.future.uroborosql.expr.Expression;
import jp.co.future.uroborosql.utils.StringFunction;

/**
 * SpringExpressionを利用した評価式パーサー
 *
 * @author H.Sugimoto
 */
public class SpelExpressionParser extends AbstractExpressionParser {
	/** 評価式のパーサー */
	private static org.springframework.expression.ExpressionParser parser;

	/** TransformContextに対するプロパティアクセサ */
	private static TransformContextPropertyAccessor transformContextPropertyAccessor;

	/**
	 * コンストラクタ
	 */
	public SpelExpressionParser() {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.AbstractExpressionParser#initialize()
	 */
	@Override
	public void initialize() {
		super.initialize();
		parser = new org.springframework.expression.spel.standard.SpelExpressionParser();
		transformContextPropertyAccessor = new TransformContextPropertyAccessor(
				getSqlConfig().getDialect().getExpressionFunction());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.ExpressionParser#parse(java.lang.String)
	 */
	@Override
	public Expression parse(final String expression) {
		return new SpringElExpression(parser.parseExpression(expression));
	}

	/**
	 * SpEL評価式
	 *
	 * @author H.Sugimoto
	 */
	private static class SpringElExpression implements Expression {
		/** 評価式 */
		private final org.springframework.expression.Expression expr;

		/**
		 * コンストラクタ
		 *
		 * @param expr 評価式
		 */
		public SpringElExpression(final org.springframework.expression.Expression expr) {
			this.expr = expr;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.expr.Expression#getValue(java.lang.Object)
		 */
		@Override
		public Object getValue(final Object context) {
			try {
				var ctx = getEvaluationContext(context);
				return expr.getValue(ctx);
			} catch (EvaluationException e) {
				throw new ExpressionRuntimeException("Acquire an object failed.[" + expr.getExpressionString() + "]",
						e);
			}
		}

		/**
		 * StandardEvaluationContextの取得.
		 *
		 * @param context StandardEvaluationContextに設定するオブジェクト
		 * @return
		 */
		private StandardEvaluationContext getEvaluationContext(final Object context) {
			var ctx = new StandardEvaluationContext(context);
			ctx.addPropertyAccessor(transformContextPropertyAccessor);
			return ctx;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.expr.Expression#dumpNode(java.lang.Object)
		 */
		@Override
		public StringBuilder dumpNode(final Object context) {
			var builder = new StringBuilder();
			if (expr != null) {
				var spel = (SpelExpression) expr;
				var root = spel.getAST();
				Set<PropertyOrFieldReference> props = new LinkedHashSet<>();
				traverseNode(root, props);

				var ctx = getEvaluationContext(context);
				var state = new ExpressionState(ctx);
				for (var prop : props) {
					var propName = prop.getName();
					if (!StringFunction.SHORT_NAME.equals(propName)) {
						try {
							var value = prop.getValue(state);
							builder.append(propName)
									.append(":[")
									.append(Objects.toString(value, null))
									.append("],");
						} catch (EvaluationException ex) {
							// ダンプ処理でシステムが止まっては困るのでスタックトレースを出して握りつぶす
							ex.printStackTrace();
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
			var spel = (SpelExpression) expr;
			var root = spel.getAST();
			Set<PropertyOrFieldReference> props = new LinkedHashSet<>();
			traverseNode(root, props);
			for (var prop : props) {
				var propName = prop.getName();
				if (!StringFunction.SHORT_NAME.equals(propName)) {
					params.add(prop.getName());
				}
			}
		}

		/**
		 * 評価式の探索
		 *
		 * @param node ノード
		 * @param params プロパティが見つかった場合に格納するSetオブジェクト
		 */
		private void traverseNode(final SpelNode node, final Set<PropertyOrFieldReference> props) {
			if (node == null) {
			} else {
				if (node instanceof PropertyOrFieldReference) {
					var prop = (PropertyOrFieldReference) node;
					props.add(prop);
				} else {
					var childCount = node.getChildCount();
					for (var i = 0; i < childCount; i++) {
						var child = node.getChild(i);
						traverseNode(child, props);
					}
				}
			}
		}
	}
}
