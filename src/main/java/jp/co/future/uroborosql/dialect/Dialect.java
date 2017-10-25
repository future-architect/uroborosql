package jp.co.future.uroborosql.dialect;

import jp.co.future.uroborosql.connection.ConnectionSupplier;

import java.sql.Driver;
import java.util.List;

/**
 * Databaseの方言を表すインタフェース
 *
 * @author H.Sugimoto
 */
public interface Dialect {
	String getName();

	default boolean accept(ConnectionSupplier supplier) {
		return supplier != null ? supplier.getDatabaseName().startsWith(getName()) : false;
	}

	/**
	 * 終端文字を削除するかどうか
	 *
	 * @return 終端文字を削除する場合<code>true</code>
	 */
	default boolean isRemoveTerminator() {
		return true;
	}
}
