/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.node;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;

import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;
import jp.co.future.uroborosql.expr.ExpressionParser;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * カッコつきバインド変数を表すノード （IN句で使用）
 *
 * @author H.Sugimoto
 */
public class ParenBindVariableNode extends ExpressionNode {
	/** バインド変数置換後にバインド変数のコメント文字列を出力するかどうか */
	private final boolean outputBindComment;

	/**
	 * コンストラクタ
	 *
	 * @param expressionParser ExpressionParser
	 * @param position 開始位置
	 * @param expression 評価式
	 * @param tokenValue トークン上の値
	 * @param outputBindComment バインドコメントを出力するかどうか
	 */
	public ParenBindVariableNode(final ExpressionParser expressionParser, final int position, final String expression,
			final String tokenValue,
			final boolean outputBindComment) {
		super(expressionParser, position, 0, expression, tokenValue);
		this.outputBindComment = outputBindComment;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		Object var = eval(transformContext);

		if (var == null) {
			throw new ParameterNotFoundRuntimeException("Parameter is not set. [" + expression + "]");
		} else if (var instanceof Iterable) {
			List<Object> list = new ArrayList<>();
			for (Object v : (Iterable<?>) var) {
				list.add(v);
			}
			bindArray(transformContext, list.toArray());
		} else if (var.getClass().isArray()) {
			bindArray(transformContext, var);
		} else {
			bindArray(transformContext, new Object[] { var });
		}
		pass();
	}

	/**
	 * 配列の値をIN句として加工してバインドする
	 *
	 * @param transformContext transformコンテキスト
	 * @param values バインドする値の配列
	 */
	private void bindArray(final TransformContext transformContext, final Object values) {
		int length = Array.getLength(values);
		if (length == 0) {
			throw new ParameterNotFoundRuntimeException("Parameter is not set. [" + expression + "]");
		}

		transformContext.addSqlPart("(?");
		transformContext.addBindVariable(Array.get(values, 0));
		for (int i = 1; i < length; i++) {
			transformContext.addSqlPart(", ?");
			transformContext.addBindVariable(Array.get(values, i));
		}
		transformContext.addSqlPart(")");
		if (outputBindComment) {
			transformContext.addSqlPart("/*").addSqlPart(expression).addSqlPart("*/");
		}
		transformContext.addBindName(expression);
	}
}