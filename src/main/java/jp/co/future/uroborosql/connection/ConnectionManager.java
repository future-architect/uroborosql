/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.Connection;

/**
 * Connectionマネージャ
 *
 * @author ota
 */
public interface ConnectionManager extends AutoCloseable {

	/**
	 * コネクション取得。
	 *
	 * @return コネクション
	 */
	Connection getConnection();

	/**
	 * コネクション取得。
	 *
	 * @param alias 取得したいDB接続の別名（エイリアス）
	 * @return コネクション
	 */
	Connection getConnection(String alias);

	/**
	 * Connectionをcloseします
	 */
	@Override
	void close();

	/**
	 * コネクションのコミットを行う
	 */
	void commit();

	/**
	 * コネクションのロールバックを行う
	 */
	void rollback();

}
