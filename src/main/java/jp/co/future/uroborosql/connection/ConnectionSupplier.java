package jp.co.future.uroborosql.connection;

import java.sql.Connection;

/**
 * JDBCコネクション提供インターフェース
 *
 * @author H.Sugimoto
 */
public interface ConnectionSupplier {

	/**
	 * コネクション取得。
	 * @return
	 */
	Connection getConnection();

	/**
	 * コネクション取得。
	 * @param alias 取得したいDB接続の別名（エイリアス）
	 * @return
	 */
	Connection getConnection(String alias);

}
