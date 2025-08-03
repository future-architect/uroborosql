/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.util.concurrent.ConcurrentHashMap;

/**
 * DB接続情報を保持するクラス
 *
 * @author H.Sugimoto
 * @since v0.19.0
 */
public abstract class ConnectionContext extends ConcurrentHashMap<String, Object> {

	/** プロパティ：自動コミット・モード */
	public static String PROPS_AUTO_COMMIT = "autocommit";
	/** プロパティ：読込み専用モード */
	public static String PROPS_READ_ONLY = "readonly";
	/** プロパティ：トランザクション分離レベル */
	public static String PROPS_TRANSACTION_ISOLATION = "transactionisolation";
	/** プロパティ：スキーマ名のキャッシュ */
	private static String PROPS_CACHE_SCHEMA = "__cache_schema__";

	/**
	 * コンストラクタ
	 */
	protected ConnectionContext() {
	}

	/**
	 * 指定したDB接続情報からAutoCommitオプションを取得
	 *
	 * @return AutoCommitを行う場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean autoCommit() {
		return (boolean) getOrDefault(PROPS_AUTO_COMMIT, false);
	}

	/**
	 * 指定したDB接続情報にAutoCommitオプションを指定
	 *
	 * @param <T> {@link ConnectionContext}の具象型
	 * @param autoCommit AutoCommitを行う場合は<code>true</code>
	 * @return {@link ConnectionContext}
	 */
	@SuppressWarnings("unchecked")
	public <T extends ConnectionContext> T autoCommit(final boolean autoCommit) {
		put(PROPS_AUTO_COMMIT, autoCommit);
		return (T) this;
	}

	/**
	 * 指定したDB接続情報からReadOnlyオプションを取得
	 *
	 * @return ReadOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean readOnly() {
		return (boolean) getOrDefault(PROPS_READ_ONLY, false);
	}

	/**
	 * 指定したDB接続情報にReadOnlyオプションを指定
	 *
	 * @param <T> {@link ConnectionContext}の具象型
	 * @param readOnly readOnlyを指定する場合は<code>true</code>
	 * @return {@link ConnectionContext}
	 */
	@SuppressWarnings("unchecked")
	public <T extends ConnectionContext> T readOnly(final boolean readOnly) {
		put(PROPS_READ_ONLY, readOnly);
		return (T) this;
	}

	/**
	 * 指定したDB接続情報からtransactionIsolationオプションを取得
	 *
	 * @see Connection#TRANSACTION_READ_UNCOMMITTED
	 * @see Connection#TRANSACTION_READ_COMMITTED
	 * @see Connection#TRANSACTION_REPEATABLE_READ
	 * @see Connection#TRANSACTION_SERIALIZABLE
	 *
	 * @return transactionIsolationの指定がない場合は<code>-1</code>. 指定がある場合はその値
	 */
	public int transactionIsolation() {
		return (int) getOrDefault(PROPS_TRANSACTION_ISOLATION, -1);
	}

	/**
	 * 指定したDB接続情報にtransactionIsolationオプションを指定
	 *
	 * @see Connection#TRANSACTION_READ_UNCOMMITTED
	 * @see Connection#TRANSACTION_READ_COMMITTED
	 * @see Connection#TRANSACTION_REPEATABLE_READ
	 * @see Connection#TRANSACTION_SERIALIZABLE
	 *
	 * @param <T> {@link ConnectionContext}の具象型
	 * @param transactionIsolation transactionIsolationオプション
	 * @return {@link ConnectionContext}
	 */
	@SuppressWarnings("unchecked")
	public <T extends ConnectionContext> T transactionIsolation(final int transactionIsolation) {
		if (Connection.TRANSACTION_READ_UNCOMMITTED == transactionIsolation
				|| Connection.TRANSACTION_READ_COMMITTED == transactionIsolation
				|| Connection.TRANSACTION_REPEATABLE_READ == transactionIsolation
				|| Connection.TRANSACTION_SERIALIZABLE == transactionIsolation) {
			put(PROPS_TRANSACTION_ISOLATION, transactionIsolation);
		} else {
			throw new IllegalArgumentException("Unsupported level [" + transactionIsolation + "]");
		}
		return (T) this;
	}

	/**
	 * スキーマ名のキャッシュオプションを取得
	 *
	 * @return スキーマ名をキャッシュする場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean cacheSchema() {
		return (boolean) getOrDefault(PROPS_CACHE_SCHEMA, false);
	}

	/**
	 * スキーマ名のキャッシュオプションを指定
	 *
	 * @param <T> {@link ConnectionContext}の具象型
	 * @param cache スキーマ名をキャッシュする場合は<code>true</code>
	 * @return {@link ConnectionContext}
	 */
	@SuppressWarnings("unchecked")
	public <T extends ConnectionContext> T cacheSchema(final boolean cache) {
		put(PROPS_CACHE_SCHEMA, cache);
		return (T) this;
	}

	/**
	 * DB接続時に渡すプロパティを設定する.
	 *
	 * @param <T> {@link ConnectionContext}の具象型
	 * @param key プロパティ名
	 * @param value 値
	 * @return {@link ConnectionContext}
	 */
	@SuppressWarnings("unchecked")
	public <T extends ConnectionContext> T set(final String key, final Object value) {
		put(key, value);
		return (T) this;
	}
}
