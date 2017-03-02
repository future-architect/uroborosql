package jp.co.future.uroborosql.tx;

import java.sql.SQLException;

/**
 * SQL実行関数インタフェース
 *
 * @author ota
 */
@FunctionalInterface
public interface SQLRunnable {
	/**
	 * SQL実行
	 *
	 * @throws SQLException SQL例外
	 */
	void run() throws SQLException;
}
