/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import java.util.concurrent.ConcurrentHashMap;

/**
 * SQL読み込みインターフェース
 *
 * @author H.Sugimoto
 */
public interface SqlLoader {
	/** パスセパレータ */
	static final String PATH_SEPARATOR = "/";

	/** SQLファイルロードのデフォルトルートパス */
	static final String DEFAULT_LOAD_PATH = "sql";

	/** デフォルトのSQLファイル拡張子 */
	static final String DEFAULT_FILE_EXTENSION = ".sql";

	/**
	 * SQL読み込み<br>
	 *
	 * @return ファイル名とSQL文字列を格納したMap
	 */
	ConcurrentHashMap<String, String> load();

	/**
	 * SQL読み込み<br>
	 *
	 * @param filePath 読み込むSQLファイルのパス。ルートパスからの相対パスを指定
	 * @return SQL文字列
	 */
	String load(String filePath);

	/**
	 * SQLが存在するかどうかを判定する
	 *
	 * @param filePath 読み込むSQLファイルのパス。ルートパスからの相対パスを指定
	 * @return 存在する場合は<code>true</code>
	 */
	boolean existSql(String filePath);

	/**
	 * SQLファイルロードパス取得<br>
	 *
	 * @return  SQLファイルロードパス
	 */
	String getLoadPath();

	/**
	 * SQLファイルロードパス設定<br>
	 *
	 * @param loadPath  SQLファイルロードパス
	 */
	void setLoadPath(String loadPath);

	/**
	 * SQLファイル拡張子取得<br>
	 *
	 * @return SQLファイル拡張子
	 */
	String getFileExtension();

	/**
	 * SQLファイル拡張子設定<br>
	 *
	 * @param fileExtension SQLファイル拡張子
	 */
	void setFileExtension(String fileExtension);

	/**
	 * SQLファイルの文字エンコーディングを取得<br>
	 *
	 * @return SQLファイルの文字エンコーディング
	 */
	String getSqlEncoding();

	/**
	 * SQLファイルの文字エンコーディングを設定<br>
	 *
	 * @param encoding SQLファイルの文字エンコーディング
	 */
	void setSqlEncoding(String encoding);

}