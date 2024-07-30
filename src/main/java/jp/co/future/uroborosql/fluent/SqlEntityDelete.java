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
	 * 発行するSQLに付与するSQL_IDを設定する
	 *
	 * @param sqlId SQL_ID文字列
	 * @return SqlEntityDelete
	 */
	SqlEntityDelete<E> sqlId(String sqlId);

	/**
	 * リトライ回数を設定する。 リトライ待機時間は0msが設定される
	 *
	 * @param count リトライ回数
	 * @return SqlEntityDelete
	 */
	SqlEntityDelete<E> retry(int count);

	/**
	 * リトライ回数を設定する
	 *
	 * @param count リトライ回数
	 * @param waitTime リトライ待機時間（ms）
	 * @return SqlEntityDelete
	 */
	SqlEntityDelete<E> retry(int count, int waitTime);

	/**
	 * 削除結果の取得（終端処理）
	 *
	 * @return 削除件数
	 */
	int count();
}
