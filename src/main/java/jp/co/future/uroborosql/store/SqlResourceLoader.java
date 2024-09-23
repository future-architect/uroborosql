/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.List;

/**
 * SQLリソースローダー インタフェース.
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public interface SqlResourceLoader {
	/**
	 * ファイル拡張子を取得します.
	 * @return ファイル拡張子
	 */
	String getFileExtension();

	/**
	 * ファイル拡張子を設定します.
	 * @param fileExtension ファイル拡張子
	 */
	void setFileExtension(final String fileExtension);

	/**
	 * Charsetを取得します.
	 * @return Charset
	 */
	Charset getCharset();

	/**
	 * Charsetを設定します.
	 * @param charset Charset
	 */
	void setCharset(final Charset charset);

	/**
	 * クラスローダーを取得します.
	 * @return クラスローダー
	 */
	ClassLoader getClassLoader();

	/**
	 * 指定した名前に対するリソースのURLを取得します.
	 * @param name リソース名
	 * @return リソースに対するURL
	 */
	URL getResource(String name);

	/**
	 * 指定した名前に対するリソースのURLを取得します.
	 * @param path リソースパス
	 * @return リソースに対するURL
	 */
	URL getResource(Path path);

	/**
	 * 指定したパス配下のSQLリソースをすべて取得します.
	 * @param rootPath SQLリソースのルートパス
	 * @return 取得したSQL情報のリスト
	 */
	List<SqlInfo> loadAllSql(final Path rootPath);

	/**
	 * 指定されたInputStreamからSQL本文を取得します. 引数のInputStreamは読み込み後クローズされます.
	 * @param is 対象のInputStream
	 * @return SQL本文
	 * @throws IOException SQLの読み込みに失敗した場合
	 */
	String loadSql(final InputStream is) throws IOException;

	/**
	 * 指定されたURLからSQL本文を取得します. 引数のURLから取得したInputStreamは読み込み後クローズされます.
	 * @param url 対象のURL
	 * @return SQL本文
	 * @throws IOException SQLの読み込みに失敗した場合
	 */
	String loadSql(final URL url) throws IOException;

}
