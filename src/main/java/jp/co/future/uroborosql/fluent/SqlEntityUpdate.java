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

	/**
	 * 更新する値の設定.
	 *
	 * @param <V> 値の型
	 * @param col 更新するカラム名（キャメルケース）
	 * @param value 更新する値
	 * @return SqlEntityUpdate
	 */
	<V> SqlEntityUpdate<E> set(final String col, final V value);

	/**
	 * 更新する値の設定.
	 *
	 * @param <V> 値の型
	 * @param col 更新するカラム名（キャメルケース）
	 * @param supplier 更新する値を提供するサプライヤ
	 * @return SqlEntityUpdate
	 */
	<V> SqlEntityUpdate<E> set(final String col, final Supplier<V> supplier);

	/**
	 * 更新する値の設定.
	 *
	 * @param <V> 値の型
	 * @param col 更新するカラム名（キャメルケース）
	 * @param value 更新する値
	 * @param sqlType SQLタイプ
	 * @return SqlEntityUpdate
	 */
	<V> SqlEntityUpdate<E> set(final String col, final V value, final int sqlType);

	/**
	 * 更新する値の設定.
	 *
	 * @param <V> 値の型
	 * @param col 更新するカラム名（キャメルケース）
	 * @param value 更新する値
	 * @param sqlType SQLタイプ
	 * @return SqlEntityUpdate
	 */
	<V> SqlEntityUpdate<E> set(final String col, final V value, final SQLType sqlType);

}
