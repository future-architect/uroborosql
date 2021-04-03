/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

/**
 * DataSourceを使用したDB接続を行う際に必要な情報を保持するクラス
 *
 * @author H.Sugimoto
 * @since v0.19.0
 */
public class DataSourceConnectionContext extends ConnectionContext {

	/** プロパティキー：データソース名 */
	public static final String PROPS_DATASOURCE_NAME = "datasource_name";

	/** デフォルトデータソース名 */
	public static final String DEFAULT_DATASOURCE_NAME = "java:comp/env/jdbc/default_datasource";

	/**
	 * コンストラクタ
	 */
	DataSourceConnectionContext() {
		this(DEFAULT_DATASOURCE_NAME);
	}

	/**
	 * コンストラクタ
	 *
	 * @param dataSourceName データソース名
	 */
	DataSourceConnectionContext(final String dataSourceName) {
		if (dataSourceName == null) {
			throw new IllegalArgumentException("dataSourceName is required but null.");
		}
		put(PROPS_DATASOURCE_NAME, dataSourceName);
	}

	/**
	 * データソース名の取得.
	 *
	 * @return データソース名
	 */
	public String dataSourceName() {
		return (String) get(PROPS_DATASOURCE_NAME);
	}

	/**
	 * データソース名の設定.
	 *
	 * @param dataSourceName データソース名
	 * @return {@link DataSourceConnectionContext}
	 */
	public DataSourceConnectionContext dataSourceName(final String dataSourceName) {
		if (dataSourceName == null) {
			throw new IllegalArgumentException("dataSourceName is required but null.");
		}
		put(PROPS_DATASOURCE_NAME, dataSourceName);
		return this;
	}

}
