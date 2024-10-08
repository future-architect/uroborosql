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
	 * 更新結果の取得（終端処理）
	 *
	 * @return 更新件数
	 */
	int count();
}
