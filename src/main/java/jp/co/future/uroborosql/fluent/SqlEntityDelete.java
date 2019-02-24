/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.fluent;

/**
 * Entity削除 実行インタフェース
 *
 * @param <E> Entity型
 * @author ota
 */
public interface SqlEntityDelete<E> extends ExtractionCondition<SqlEntityDelete<E>> {
	/**
	 * 削除結果の取得（終端処理）
	 *
	 * @return 削除件数
	 */
	int count();
}
