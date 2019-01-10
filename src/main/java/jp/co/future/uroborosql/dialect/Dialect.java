/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import jp.co.future.uroborosql.connection.ConnectionSupplier;

/**
 * Databaseの方言を表すインタフェース
 *
 * @author H.Sugimoto
 */
public interface Dialect {
	/**
	 * データベースを判別するための文字列を取得する。
	 *
	 * @return データベースを判別するための文字列
	 */
	String getDatabaseName();

	default boolean accept(final ConnectionSupplier supplier) {
		return supplier != null && supplier.getDatabaseName().startsWith(getDatabaseName());
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
	 * リトライする前にSavepointまでロールバックするかどうか
	 *
	 * @return ロールバックする場合<code>true</code>
	 */
	default boolean isRollbackToSavepointBeforeRetry() { return false; }

	/**
	 * BULK INSERTをサポートするかどうか
	 * @return BULK INSERTをサポートする場合<code>true</code>
	 */
	default boolean supportsBulkInsert() {
		return false;
	}

	/**
	 * Dialect名を取得する
	 *
	 * @return Dialect名
	 */
	String getDialectName();

}
