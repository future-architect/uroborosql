/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.fluent;

import java.sql.SQLType;
import java.util.function.Supplier;

/**
 * Entity Update 実行インタフェース
 *
 * @param <E> Entity型
 * @author H.Sugimoto
 */
public interface SqlEntityUpdate<E> extends ExtractionCondition<SqlEntityUpdate<E>> {
	/**
	 * 更新結果の取得（終端処理）
	 *
	 * @return 更新件数
	 */
	int count();

	<V> SqlEntityUpdate<E> set(final String paramName, final V value);

	<V> SqlEntityUpdate<E> set(final String paramName, final Supplier<V> supplier);

	<V> SqlEntityUpdate<E> set(final String paramName, final V value, final int sqlType);

	<V> SqlEntityUpdate<E> set(final String paramName, final V value, final SQLType sqlType);

}
