/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import java.net.URL;
import java.nio.file.Path;
import java.util.List;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.log.support.ServiceLoggingSupport;

/**
 * SQLリソース管理インターフェース
 *
 * @author H.Sugimoto
 */
public interface SqlResourceManager extends ServiceLoggingSupport {
	/** SQLファイルロードのデフォルトルートパス */
	String DEFAULT_LOAD_PATH = "sql";

	/**
	 * 初期化<br>
	 */
	void initialize();

	/**
	 * キャッシュクリア処理<br>
	 */
	void clearCache();

	/**
	 * SQL文取得<br>
	 * @param sqlName SQL名
	 * @return SQL文
	 */
	String getSql(String sqlName);

	/**
	 * SQLが存在するかどうかを判定する
	 *
	 * @param sqlName SQL名
	 * @return 存在する場合は<code>true</code>
	 */
	boolean existSql(String sqlName);

	/**
	 * 指定したSQL名のSQLをロードし直します.
	 *
	 * @param sqlName SQL名
	 * @return sqlNameに対するSQLが存在する場合は<code>true</code>
	 */
	boolean reloadSql(String sqlName);

	/**
	 * SqlNameを与えられたPathから生成する<br>
	 *
	 * <pre>
	 * SqlNameは以下のルールで生成する
	 * 1. loadPathで指定されたフォルダの下のフォルダ名とファイル名を"/"でつなげた文字列とする
	 * 2. loadPathの直下にdialectと一致するフォルダがある場合は、dialectフォルダの下のフォルダとファイル名を"/"でつなげた文字列とする
	 * 3. loadPathが複数指定されていて、同名のsqlNameがある場合、loadPathの並び順で先になるものを優先する
	 *
	 * ex)
	 *
	 *  sql/
	 *    example/
	 *      test1.sql
	 *      test2.sql
	 *      test3.sql
	 *    oracle/
	 *      example/
	 *        test1.sql
	 *    postgresql/
	 *      example/
	 *        test2.sql
	 *  secondary_sql/
	 *    example/
	 *      test3.sql
	 *      test4.sql
	 *
	 *   上記のフォルダ構成で
	 *   - loadPath=sql, dialect=oracleの場合は以下のSqlNameが生成される
	 *     example/test1 ( 実際はoracle/example/test1 )
	 *     example/test2
	 *   - loadPath=sql, dialect=postgresqlの場合は以下のSqlNameが生成される
	 *     example/test1
	 *     example/test2 ( 実際はpostgresql/example/test2 )
	 *   - loadPath=[sql, secondary_sql], dialect=postgresqlの場合は以下のSqlNameが生成される
	 *     example/test1
	 *     example/test2 ( 実際はpostgresql/example/test2 )
	 *     example/test3
	 *     example/test4
	 *   - loadPath=[secondary_sql, sql], dialect=postgresqlの場合は以下のSqlNameが生成される
	 *     example/test1
	 *     example/test2 ( 実際はpostgresql/example/test2 )
	 *     example/test3 ( 実際はsecondary_sql/example/test3 )
	 *     example/test4
	 *
	 * </pre>
	 *
	 * @param path Path ファイルパス
	 * @return SqlName SqlName
	 */
	String getSqlName(final Path path);

	/**
	 * SQL名に対して現在有効なURLを取得する
	 *
	 * @param sqlName SQL名
	 * @return 現在有効なURL。存在しないSQL名の場合はUroborosqlRuntimeExceptionがスローされる
	 */
	URL getSqlUrl(String sqlName);

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