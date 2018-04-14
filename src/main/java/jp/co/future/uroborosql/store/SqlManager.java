/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import java.util.List;

/**
 * SQL管理インターフェース
 *
 * @author H.Sugimoto
 */
public interface SqlManager {

	/**
	 * 初期化<br>
	 */
	void initialize();

	/**
	 * SQL文取得<br>
	 * @param sqlPath ルートパスからの相対パス
	 * @return SQL文
	 */
	String getSql(String sqlPath);

	/**
	 * SQLが存在するかどうかを判定する
	 *
	 * @param sqlPath ルートパスからの相対パス
	 * @return 存在する場合は<code>true</code>
	 */
	boolean existSql(String sqlPath);

	/**
	 * ロードしたSQLのパス一覧を取得する
	 * @return ロードしたSQLパス一覧
	 */
	List<String> getSqlPathList();

	/**
	 * SqlLoader の取得<br>
	 * @return SqlLoader
	 */
	SqlLoader getSqlLoader();

	/**
	 * SqlLoader の設定<br>
	 *
	 * @param sqlLoader SqlLoader
	 */
	void setSqlLoader(SqlLoader sqlLoader);

	/**
	 * 起動時にSQLファイルをキャッシュするかどうかを取得する
	 *
	 * @return SQLをキャッシュする場合<code>true</code>
	 */
	boolean isCache();

	/**
	 * 起動時にSQLファイルをキャッシュするかどうか<BR>
	 *
	 * @param cache SQLをキャッシュする場合<code>true</code>
	 */
	void setCache(boolean cache);

}