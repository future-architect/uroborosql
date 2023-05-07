/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.expr;

import jp.co.future.uroborosql.config.SqlConfigAware;

/**
 * 評価式のパーサー
 *
 * @author H.Sugimoto
 */
public interface ExpressionParser extends SqlConfigAware {

	/**
	 * 初期化処理
	 */
	void initialize();

	/**
	 * 文字列を評価し、評価式を取得する.
	 *
	 * @param expression 評価を行う文字列
	 * @return 評価式
	 */
	Expression parse(String expression);

	/**
	 * 評価対象文字列がプロパティアクセスを表しているかどうかを判定する.
	 *
	 * @param expression 評価対象文字列
	 * @return 評価対象文字列がプロパティアクセスを表している場合<code>true</code>
	 */
	default boolean isPropertyAccess(final String expression) {
		if (expression == null || expression.isEmpty()) {
			return true;
		} else {
			// 中かっこ（）がある場合はメソッドとして扱う（paramName.methodName(args...))
			return !expression.contains("(") || !expression.contains(")");
		}
	}
}
