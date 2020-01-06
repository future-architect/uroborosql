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
		super();
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
	private class SpringElExpression implements Expression {
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
				StandardEvaluationContext ctx = getEvaluationContext(context);
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
			StandardEvaluationContext ctx = new StandardEvaluationContext(context);
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
			StringBuilder builder = new StringBuilder();
			if (expr != null) {
				SpelExpression spel = (SpelExpression) expr;
				SpelNode root = spel.getAST();
				Set<PropertyOrFieldReference> props = new LinkedHashSet<>();
				traverseNode(root, props);

				StandardEvaluationContext ctx = getEvaluationContext(context);
				ExpressionState state = new ExpressionState(ctx);
				for (PropertyOrFieldReference prop : props) {
					String propName = prop.getName();
					if (!StringFunction.SHORT_NAME.equals(propName)) {
						try {
							Object value = prop.getValue(state);
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
			SpelExpression spel = (SpelExpression) expr;
			SpelNode root = spel.getAST();
			Set<PropertyOrFieldReference> props = new LinkedHashSet<>();
			traverseNode(root, props);
			for (PropertyOrFieldReference prop : props) {
				String propName = prop.getName();
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
				return;
			} else {
				if (node instanceof PropertyOrFieldReference) {
					PropertyOrFieldReference prop = (PropertyOrFieldReference) node;
					props.add(prop);
				} else {
					int childCount = node.getChildCount();
					for (int i = 0; i < childCount; i++) {
						SpelNode child = node.getChild(i);
						traverseNode(child, props);
					}
				}
			}
		}
	}
}
