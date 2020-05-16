/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

/**
 * {@link ConnectionContext} を生成するためのビルダークラス
 *
 * @author H.Sugimoto
 * @since v0.19.0
 */
public final class ConnectionContextBuilder {

	/**
	 * コンストラクタ
	 */
	private ConnectionContextBuilder() {
	}

	/**
	 * {@link DataSourceConnectionContext} を生成する.
	 *
	 * @return {@link DataSourceConnectionContext}
	 */
	public static DataSourceConnectionContext dataSource() {
		return new DataSourceConnectionContext();
	}

	/**
	 * {@link DataSourceConnectionContext} を生成する.
	 *
	 * @param dataSourceName データソース名
	 * @return {@link DataSourceConnectionContext}
	 */
	public static DataSourceConnectionContext dataSource(final String dataSourceName) {
		return new DataSourceConnectionContext(dataSourceName);
	}

	/**
	 * {@link JdbcConnectionContext} を生成する.
	 *
	 * @param url 接続URL （必須）
	 * @return {@link JdbcConnectionContext}
	 */
	public static JdbcConnectionContext jdbc(final String url) {
		return new JdbcConnectionContext(url);
	}

	/**
	 * {@link JdbcConnectionContext} を生成する.
	 *
	 * @param url 接続URL （必須）
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 * @return {@link JdbcConnectionContext}
	 */
	public static JdbcConnectionContext jdbc(final String url, final String user, final String password) {
		return new JdbcConnectionContext(url, user, password);
	}

	/**
	 * {@link JdbcConnectionContext} を生成する.
	 *
	 * @param url 接続URL （必須）
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 * @param schema 接続スキーマ
	 * @return {@link JdbcConnectionContext}
	 */
	public static JdbcConnectionContext jdbc(final String url, final String user, final String password,
			final String schema) {
		return new JdbcConnectionContext(url, user, password, schema);
	}
}
