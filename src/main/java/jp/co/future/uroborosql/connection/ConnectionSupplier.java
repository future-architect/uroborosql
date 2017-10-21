package jp.co.future.uroborosql.connection;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;

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
			}catch (SQLException ex){
				throw new UroborosqlSQLException(ex);
			}
		}
	}
}
