/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parser;

import jp.co.future.uroborosql.coverage.PassedRoute;
import jp.co.future.uroborosql.node.Node;

/**
 * Transformコンテキストの変換器
 *
 * @author H.Sugimoto
 */
public class ContextTransformer {
	/** ルートノード */
	private final Node root;

	/**
	 * コンストラクタ
	 *
	 * @param root ルートノード
	 */
	public ContextTransformer(final Node root) {
		this.root = root;
	}

	/**
	 * Transformコンテキストの変換を実施する
	 *
	 * @param transformContext Transformコンテキスト
	 */
	public void transform(final TransformContext transformContext) {
		root.accept(transformContext);
	}

	/**
	 * ルートノードの取得
	 *
	 * @return ルートノード
	 */
	public Node getRoot() {
		return root;
	}

	/**
	 * 変換したSQLで評価された分岐を表す文字列を取得する
	 *
	 * @return 評価結果を表す文字列
	 */
	public PassedRoute getPassedRoute() {
		var passed = new PassedRoute();
		root.passed(passed);
		return passed;
	}

}
