/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import java.sql.JDBCType;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.mapping.JavaType;
import jp.co.future.uroborosql.utils.StringFunction;

/**
 * Databaseの方言を表すインタフェース
 *
 * @author H.Sugimoto
 */
public interface Dialect {

	/**
	 * OGNL式内で使用する式関数を取得する
	 *
	 * @return 式関数
	 */
	StringFunction getExpressionFunction();

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
	 * SELECT句のORDER BY でNULL値の順序を指定できるか（NULLS FIRST/LAST）
	 *
	 * @return NULL値の順序指定ができる場合<code>true</code>
	 */
	default boolean supportsNullValuesOrdering() {
		return false;
	}

	/**
	 * LIMIT句（とOFFSET句）を取得する
	 *
	 * @param limit limit
	 * @param offset offset
	 * @return LIMIT句（とOFFSET句）を表す文字列
	 */
	String getLimitClause(long limit, long offset);

	/**
	 * LIKE 演算子のパターン文字列をエスケープする
	 *
	 * @param pattern パターン文字列
	 * @return エスケープ後のパターン文字列
	 */
	String escapeLikePattern(CharSequence pattern);

	/**
	 * {@link JDBCType} を変換するJava型を取得する
	 *
	 * @param jdbcType JDBCType
	 * @param jdbcTypeName データベース固有の型名
	 * @return 変換するJava型
	 */
	JavaType getJavaType(JDBCType jdbcType, String jdbcTypeName);

	/**
	 * {@link JDBCType} を変換するJava型を取得する
	 *
	 * @param jdbcType JDBCTypeを表す数値
	 * @param jdbcTypeName データベース固有の型名
	 * @return 変換するJava型
	 */
	JavaType getJavaType(int jdbcType, String jdbcTypeName);

	/**
	 * Databaseの種別を表す名前を取得する
	 *
	 * @return Database種別名
	 */
	String getDatabaseType();

	/**
	 * LIKE句で指定するエスケープキャラクタを取得する
	 *
	 * @return エスケープキャラクタ
	 */
	char getEscapeChar();

}
