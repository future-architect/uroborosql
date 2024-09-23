/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import java.nio.file.Path;

/**
 * SQLファイルの情報を保持するオブジェクト
 */
public class SqlInfo {
	/** zip, jar内のファイルのscheme */
	public static final String SCHEME_JAR = "jar";

	/** ファイルシステム上のファイルのscheme */
	public static final String SCHEME_FILE = "file";

	/** sqlNameに対応するPath. */
	private final Path path;

	/** 読み込みを行ったSQLルートパス */
	private final Path rootPath;

	/** 読み込んだファイルのscheme */
	private final String scheme;

	/** SQLファイルの内容.*/
	private final String sqlBody;

	/**
	 * コンストラクタ
	 * @param path Path
	 * @param rootPath 読み込みを行ったSQLルートパス
	 * @param scheme 読み込んだファイルのscheme
	 * @param sqlBody SqlBody
	 */
	public SqlInfo(final Path path, final Path rootPath, final String scheme, final String sqlBody) {
		this.path = path;
		this.rootPath = rootPath;
		this.scheme = scheme;
		this.sqlBody = sqlBody;
	}

	/**
	 * sqlNameに対応するPath.を取得します.
	 * @return sqlNameに対応するPath.
	 */
	public Path getPath() {
		return path;
	}

	/**
	 * 読み込みを行ったSQLルートパスを取得します.
	 * @return 読み込みを行ったSQLルートパス
	 */
	public Path getRootPath() {
		return rootPath;
	}

	/**
	 * 読み込んだファイルのschemeを取得します.
	 * @return 読み込んだファイルのscheme
	 */
	public String getScheme() {
		return scheme;
	}

	/**
	 * SQLファイルの内容.を取得します.
	 * @return SQLファイルの内容.
	 */
	public String getSqlBody() {
		return sqlBody;
	}

}