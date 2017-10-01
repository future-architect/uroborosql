package jp.co.future.uroborosql.dialect;

import java.sql.Driver;
import java.util.List;

/**
 * Databaseの方言を表すインタフェース
 *
 * @author H.Sugimoto
 */
public interface Dialect {
	String getName();

	String getDriverClassName();

	default boolean accept(Driver driver) {
		return driver != null ? driver.getClass().getName().startsWith(getDriverClassName()) : false;
	}

	/**
	 * 終端文字を削除するかどうか
	 *
	 * @return 終端文字を削除する場合<code>true</code>
	 */
	default boolean isRemoveTerminator() {
		return true;
	}

	/**
	 * リトライするSQLエラーコードのリストを取得する
	 *
	 * @return リトライするSQLエラーコードリスト
	 */
	List<String> getSqlRetryCodes();

	/**
	 * リトライするSQLエラーコードを設定する
	 *
	 * @param sqlRetryCodes リトライするSQLエラーコードリスト
	 */
	void setSqlRetryCodes(List<String> sqlRetryCodes);
}
