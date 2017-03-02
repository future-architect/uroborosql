package jp.co.future.uroborosql.tx;

import java.sql.SQLException;

/**
 * SQL供給関数インタフェース
 *
 * @author ota
 *
 * @param <T> 処理ブロックの型
 */
@FunctionalInterface
public interface SQLSupplier<T> {
	/**
	 * 処理ブロックの取得
	 * @return 処理ブロック
	 * @throws SQLException SQL例外
	 */
	T get() throws SQLException;
}
