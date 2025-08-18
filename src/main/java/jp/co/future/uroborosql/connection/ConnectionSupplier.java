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
	 * @param ctx DB接続に使用するコンテキスト
	 * @return コネクション
	 */
	Connection getConnection(ConnectionContext ctx);

	/**
	 * デフォルトのDB接続情報にスキーマ名のキャッシュオプションを指定
	 *
	 * @param cache スキーマ名をキャッシュする場合は<code>true</code>
	 */
	void setDefaultCacheSchema(final boolean cache);

	/**
	 * デフォルトのDB接続情報にスキーマ名の固定オプションを指定
	 *
	 * @param fixed スキーマ名を固定する場合は<code>true</code>
	 */
	void setDefaultFixSchema(final boolean fixed);

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
