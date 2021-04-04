/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
/**
 *
 */
package jp.co.future.uroborosql.fluent;

/**
 * SQL Update 実行インタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlUpdate extends SqlFluent<SqlUpdate> {

	/**
	 * これまでに追加されたパラメータ群をバッチパラメータに格納する
	 *
	 * @return SqlUpdate
	 */
	@Deprecated
	SqlUpdate addBatch();

	/**
	 * 更新結果の取得（終端処理）
	 *
	 * @return 更新件数
	 */
	int count();

	/**
	 * 一括更新結果の取得（終端処理）
	 *
	 * addBatch()で登録したバッチパラメータを使った一括更新処理を行う。
	 *
	 * @return 更新件数の配列
	 */
	@Deprecated
	int[] batch();
}
