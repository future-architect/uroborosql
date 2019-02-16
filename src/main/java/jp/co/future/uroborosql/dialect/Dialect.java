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
	default boolean isRollbackToSavepointBeforeRetry() {
		return false;
	}

	/**
	 * BULK INSERTをサポートするかどうか
	 * @return BULK INSERTをサポートする場合<code>true</code>
	 */
	default boolean supportsBulkInsert() {
		return false;
	}

	/**
	 * LIMIT 句をサポートするかどうか
	 * @return LIMIT句をサポートする場合は<code>true</code>
	 */
	default boolean supportsLimitClause() {
		return false;
	}

	/**
	 * LIMIT句（とOFFSET句）を取得する
	 * @param limit limit
	 * @param offset offset
	 * @return LIMIT句（とOFFSET句）を表す文字列
	 */
	String getLimitClause(long limit, long offset);

	/**
	 * LIKE 演算子のパターン文字列をエスケープする
	 * @param pattern パターン文字列
	 * @return エスケープ後のパターン文字列
	 */
	String escapeLikePattern(CharSequence pattern);

	/**
	 * Databaseの種別を表す名前を取得する
	 *
	 * @return Database種別名
	 */
	String getDatabaseType();

}
