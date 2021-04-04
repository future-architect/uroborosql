/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import java.util.List;

import jp.co.future.uroborosql.dialect.Dialect;

/**
 * SQLリソース管理インターフェース
 *
 * @author H.Sugimoto
 */
public interface SqlResourceManager {

	/**
	 * 初期化<br>
	 */
	void initialize();

	/**
	 * 停止処理<br>
	 */
	void shutdown();

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
	 * Dialectの取得
	 *
	 * @return Dialect
	 */
	Dialect getDialect();

	/**
	 * Dialectの設定
	 *
	 * @param dialect Dialect
	 */
	void setDialect(Dialect dialect);

}