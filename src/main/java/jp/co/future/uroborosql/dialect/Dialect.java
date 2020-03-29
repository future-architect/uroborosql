/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import java.sql.SQLType;
import java.util.List;
import java.util.Set;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.enums.ForUpdateType;
import jp.co.future.uroborosql.mapping.JavaType;
import jp.co.future.uroborosql.utils.StringFunction;

/**
 * Databaseの方言を表すインタフェース
 *
 * @author H.Sugimoto
 */
public interface Dialect {
	/** エスケープ文字の置換文字列 */
	String PARAM_KEY_ESCAPE_CHAR = "ESC_CHAR";

	/**
	 * 評価式内で使用する式関数を取得.
	 *
	 * @return 式関数
	 */
	StringFunction getExpressionFunction();

	/**
	 * データベースを判別するための文字列を取得.
	 *
	 * @return データベースを判別するための文字列
	 */
	String getDatabaseName();

	default boolean accept(final ConnectionSupplier supplier) {
		return supplier != null && supplier.getDatabaseName().startsWith(getDatabaseName());
	}

	/**
	 * 終端文字を削除するかどうか.
	 *
	 * @return 終端文字を削除する場合<code>true</code>
	 */
	default boolean isRemoveTerminator() {
		return true;
	}

	/**
	 * リトライする前にSavepointまでロールバックするかどうか.
	 *
	 * @return ロールバックする場合<code>true</code>
	 */
	default boolean isRollbackToSavepointBeforeRetry() {
		return false;
	}

	/**
	 * BULK INSERTをサポートするかどうか.
	 *
	 * @return BULK INSERTをサポートする場合<code>true</code>
	 */
	default boolean supportsBulkInsert() {
		return false;
	}

	/**
	 * LIMIT 句をサポートするかどうか.
	 *
	 * @return LIMIT句をサポートする場合は<code>true</code>
	 */
	default boolean supportsLimitClause() {
		return false;
	}

	/**
	 * オプティマイザーヒントをサポートするかどうか.
	 *
	 * @return オプティマイザーヒントの指定をサポートする場合は<code>true</code>
	 */
	default boolean supportsOptimizerHints() {
		return false;
	}

	/**
	 * SELECT句のORDER BY でNULL値の順序を指定できるか（NULLS FIRST/LAST）.
	 *
	 * @return NULL値の順序指定ができる場合<code>true</code>
	 */
	default boolean supportsNullValuesOrdering() {
		return false;
	}

	/**
	 * データベースのIDカラムを使用したID自動採番をサポートしているか.
	 *
	 * @return データベースのIDカラムを使用したID自動採番をサポートしている場合<code>true</code>
	 */
	default boolean supportsIdentity() {
		return true;
	}

	/**
	 * データベースのシーケンスを使用したID自動採番をサポートしているか.
	 *
	 * @return データベースのシーケンスを使用したID自動採番をサポートしている場合<code>true</code>
	 */
	default boolean supportsSequence() {
		return true;
	}

	/**
	 * 明示的な行ロックをサポートしているか.
	 *
	 * @return 明示的な行ロックをサポートしている場合<code>true</code>
	 */
	default boolean supportsForUpdate() {
		return true;
	}

	/**
	 * 明示的な行ロック（待機なし）をサポートしているか.
	 *
	 * @return 明示的な行ロック（待機なし）をサポートしている場合<code>true</code>
	 */
	default boolean supportsForUpdateNoWait() {
		return true;
	}

	/**
	 * 明示的な行ロック（待機あり）をサポートしているか.
	 *
	 * @return 明示的な行ロック（待機あり）をサポートしている場合<code>true</code>
	 */
	default boolean supportsForUpdateWait() {
		return true;
	}

	String getSequenceNextValSql(String sequenceName);

	/**
	 * LIMIT句（とOFFSET句）を取得.
	 *
	 * @param limit limit
	 * @param offset offset
	 * @return LIMIT句（とOFFSET句）を表す文字列
	 */
	String getLimitClause(long limit, long offset);

	/**
	 * LIKE 演算子のパターン文字列のエスケープ.
	 *
	 * @param pattern パターン文字列
	 * @return エスケープ後のパターン文字列
	 */
	String escapeLikePattern(CharSequence pattern);

	/**
	 * {@link SQLType} を変換するJava型を取得.
	 *
	 * @param sqlType SQLType
	 * @param sqlTypeName データベース固有の型名
	 * @return 変換するJava型
	 */
	JavaType getJavaType(SQLType sqlType, String sqlTypeName);

	/**
	 * {@link SQLType} を変換するJava型を取得.
	 *
	 * @param sqlType SQLTypeを表す数値
	 * @param sqlTypeName データベース固有の型名
	 * @return 変換するJava型
	 */
	JavaType getJavaType(int sqlType, String sqlTypeName);

	/**
	 * Databaseの種別を表す名前を取得.
	 *
	 * @return Database種別名
	 */
	String getDatabaseType();

	/**
	 * LIKE句で指定するエスケープキャラクタを取得.
	 *
	 * @return エスケープキャラクタ
	 */
	char getEscapeChar();

	/**
	 * FOR UPDATE句の文字列をSQLに追加.
	 *
	 * @param sql 追加対象のSQL文
	 * @param forUpdateType forUpdateのタイプ
	 * @param waitSeconds 待機時間
	 * @return FOR UPDATE句を追加したSQL文
	 */
	StringBuilder addForUpdateClause(StringBuilder sql, ForUpdateType forUpdateType, int waitSeconds);

	/**
	 * Optimizer Hint文字列をSQLに追加.
	 *
	 * @param sql 追加対象のSQL文
	 * @param hints ヒント句のリスト
	 * @return Hint句を追加したSQL文
	 */
	StringBuilder addOptimizerHints(StringBuilder sql, List<String> hints);

	/**
	 * 乗除を行うためのSQL文字列を取得.
	 *
	 * @param dividend 除算される値
	 * @param divisor 除算する値
	 * @return 乗除を行うためのSQL文字列
	 */
	default String getModLiteral(final String dividend, final String divisor) {
		return dividend + " % " + divisor;
	}

	Set<String> getPessimisticLockingErrorCodes();
}
