/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.config;

/**
 * SqlConfigに対するアクセスインタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlConfigAware {

	/**
	 * SqlConfigの設定.
	 *
	 * @param sqlConfig SqlConfig
	 */
	void setSqlConfig(SqlConfig sqlConfig);

	/**
	 * SqlConfigの取得.
	 *
	 * @return SqlConfig
	 */
	SqlConfig getSqlConfig();
}
