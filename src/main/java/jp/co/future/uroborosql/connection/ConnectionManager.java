package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.sql.SQLException;

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
	void close() throws SQLException;

	/**
	 * コネクションのコミットを行う
	 *
	 * @throws SQLException SQL例外
	 */
	void commit() throws SQLException;

	/**
	 * コネクションのロールバックを行う
	 *
	 * @throws SQLException SQL例外
	 */
	void rollback() throws SQLException;

}
