/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.expr;

import java.util.Set;

/**
 * 評価式を表すクラス
 *
 * @author H.Sugimoto
 */
public interface Expression {
	/**
	 * 評価式を使用してcontextから値を取得する.
	 *
	 * @param context 評価式を適用するcontext
	 * @return 評価の結果取得したオブジェクト
	 */
	Object getValue(Object context);

	/**
	 * 評価式で使われるプロパティとその値を出力する.
	 *
	 * @param context 評価式を適用するcontext
	 * @return プロパティと値の出力結果を保持するBuilder
	 */
	StringBuilder dumpNode(Object context);

	/**
	 * 評価式に含まれるプロパティの集合を取得する.
	 *
	 * @param params プロパティを格納する集合
	 */
	void collectParams(final Set<String> params);

}
