/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.Connection;
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
	 * @param ctx DB接続に使用するコンテキスト
	 * @return コネクション
	 */
	Connection getConnection(ConnectionContext ctx);

	/**
	 * 接続しているDBプロダクト名+ バージョンを取得する
	 *
	 * @return DatabaseName + "-" + DatabaseVersion
	 */
	default String getDatabaseName() {
		Connection conn = null;
		try {
			conn = getConnection();
			var metaData = conn.getMetaData();
			return metaData.getDatabaseProductName() + "-" + metaData.getDatabaseMajorVersion() + "."
					+ metaData.getDatabaseMinorVersion();
		} catch (SQLException ex) {
			throw new UroborosqlSQLException(ex);
		} finally {
			try {
				if (conn != null && !conn.isClosed()) {
					conn.close();
				}
			} catch (SQLException ex) {
				throw new UroborosqlSQLException(ex);
			}
		}
	}
}
