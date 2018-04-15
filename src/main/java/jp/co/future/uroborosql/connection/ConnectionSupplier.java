/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;

import jp.co.future.uroborosql.exception.UroborosqlSQLException;

/**
 * JDBCコネクション提供インターフェース
 *
 * @author H.Sugimoto
 */
public interface ConnectionSupplier {

	/**
	 * コネクション取得。
	 * @return コネクション
	 */
	Connection getConnection();

	/**
	 * コネクション取得。
	 * @param alias 取得したいDB接続の別名（エイリアス）
	 * @return コネクション
	 */
	Connection getConnection(String alias);

	/**
	 * 接続しているDBプロダクト名+ バージョンを取得する
	 *
	 * @return DatabaseName + "-" + DatabaseVersion
	 */
	default String getDatabaseName() {
		Connection conn = null;
		try {
			conn = getConnection();
			DatabaseMetaData metaData = conn.getMetaData();
			return metaData.getDatabaseProductName() + "-" + metaData.getDatabaseProductVersion();
		} catch (SQLException ex) {
			throw new UroborosqlSQLException(ex);
		} finally {
			try {
				if (conn != null) {
					conn.close();
				}
			} catch (SQLException ex) {
				throw new UroborosqlSQLException(ex);
			}
		}
	}
}
