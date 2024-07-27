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
	 * 発行するSQLに付与するSQL_IDを設定する
	 *
	 * @param sqlId SQL_ID文字列
	 * @return SqlEntityUpdate
	 */
	SqlEntityUpdate<E> sqlId(String sqlId);

	/**
	 * リトライ回数を設定する。 リトライ待機時間は0msが設定される
	 *
	 * @param count リトライ回数
	 * @return SqlEntityUpdate
	 */
	SqlEntityUpdate<E> retry(int count);

	/**
	 * リトライ回数を設定する
	 *
	 * @param count リトライ回数
	 * @param waitTime リトライ待機時間（ms）
	 * @return SqlEntityUpdate
	 */
	SqlEntityUpdate<E> retry(int count, int waitTime);

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
	 * @param supplier 更新する値を提供するSupplier
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
