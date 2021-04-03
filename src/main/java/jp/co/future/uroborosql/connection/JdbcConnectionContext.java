/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.util.Properties;

/**
 * JDBC接続を行う際に必要な情報を保持するクラス
 *
 * @author H.Sugimoto
 * @since v0.19.0
 */
public class JdbcConnectionContext extends ConnectionContext {

	/** プロパティキー：JDBC URL */
	public static final String PROPS_JDBC_URL = "jdbc.url";
	/** プロパティキー：JDBC 接続ユーザ名 */
	public static final String PROPS_JDBC_USER = "jdbc.user";
	/** プロパティキー：JDBC 接続パスワード */
	public static final String PROPS_JDBC_PASSWORD = "jdbc.password";
	/** プロパティキー：JDBC 接続スキーマ */
	public static final String PROPS_JDBC_SCHEMA = "jdbc.schema";

	/**
	 * コンストラクタ
	 *
	 * @param url 接続URL （必須）
	 */
	JdbcConnectionContext(final String url) {
		this(url, null, null, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param url 接続URL （必須）
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 */
	JdbcConnectionContext(final String url, final String user, final String password) {
		this(url, user, password, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param url 接続URL （必須）
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 * @param schema 接続スキーマ
	 */
	JdbcConnectionContext(final String url, final String user, final String password, final String schema) {
		if (url == null) {
			throw new IllegalArgumentException("url is required but null.");
		}
		put(PROPS_JDBC_URL, url);

		if (user != null) {
			put(PROPS_JDBC_USER, user);
		}
		if (password != null) {
			put(PROPS_JDBC_PASSWORD, password);
		}
		if (schema != null) {
			put(PROPS_JDBC_SCHEMA, schema);
		}
	}

	/**
	 * 接続URLの取得.
	 *
	 * @return 接続URL
	 */
	public String url() {
		return (String) get(PROPS_JDBC_URL);
	}

	/**
	 * 接続URLの設定.
	 *
	 * @param url 接続URL
	 * @return {@link JdbcConnectionContext}
	 */
	public JdbcConnectionContext url(final String url) {
		if (url == null) {
			throw new IllegalArgumentException("url is required but null.");
		}
		put(PROPS_JDBC_URL, url);
		return this;
	}

	/**
	 * 接続ユーザ名の取得.
	 *
	 * @return 接続ユーザ名
	 */
	public String user() {
		return (String) getOrDefault(PROPS_JDBC_USER, null);
	}

	/**
	 * 接続ユーザ名の設定.
	 *
	 * @param user 接続ユーザ名
	 * @return {@link JdbcConnectionContext}
	 */
	public JdbcConnectionContext user(final String user) {
		if (user != null) {
			put(PROPS_JDBC_USER, user);
		}
		return this;
	}

	/**
	 * 接続パスワードの取得.
	 *
	 * @return 接続パスワード
	 */
	public String password() {
		return (String) getOrDefault(PROPS_JDBC_PASSWORD, null);
	}

	/**
	 * 接続パスワードの設定.
	 *
	 * @param password 接続パスワード
	 * @return {@link JdbcConnectionContext}
	 */
	public JdbcConnectionContext password(final String password) {
		if (password != null) {
			put(PROPS_JDBC_PASSWORD, password);
		}
		return this;
	}

	/**
	 * 接続スキーマの取得.
	 *
	 * @return 接続スキーマ
	 */
	public String schema() {
		return (String) getOrDefault(PROPS_JDBC_SCHEMA, null);
	}

	/**
	 * 接続スキーマの設定.
	 *
	 * @param schema 接続スキーマ
	 * @return {@link JdbcConnectionContext}
	 */
	public JdbcConnectionContext schema(final String schema) {
		if (schema != null) {
			put(PROPS_JDBC_SCHEMA, schema);
		}
		return this;
	}

	/**
	 * JDBC接続時に渡すプロパティの取得
	 *
	 * @return JDBC接続時に渡すプロパティ
	 */
	public Properties toProperties() {
		Properties props = new Properties();
		// DriverManagerに渡す際、最低限必要な情報（user, password）を設定
		if (user() != null) {
			props.put("user", user());
		}
		if (password() != null) {
			props.put("password", password());
		}

		entrySet().forEach(e -> {
			String key = e.getKey();
			if (e.getValue() != null &&
					key != PROPS_JDBC_URL &&
					key != PROPS_JDBC_USER &&
					key != PROPS_JDBC_PASSWORD &&
					key != PROPS_JDBC_SCHEMA &&
					key != PROPS_AUTO_COMMIT &&
					key != PROPS_READ_ONLY &&
					key != PROPS_TRANSACTION_ISOLATION) {
				props.put(key, e.getValue());
			}
		});
		return props;
	}

}
