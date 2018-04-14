/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.tx;

/**
 * SQL供給関数インタフェース
 *
 * @author ota
 *
 * @param <T> 処理ブロックの型
 */
@FunctionalInterface
public interface SQLSupplier<T> {
	/**
	 * 処理ブロックの取得
	 * @return 処理ブロック
	 */
	T get();
}
