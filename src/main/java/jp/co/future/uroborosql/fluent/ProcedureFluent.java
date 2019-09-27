/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.fluent;

import java.sql.SQLType;

/**
 * Fluent API を使用してProcedureへのアクセスを行うためのインタフェース
 *
 * @author H.Sugimoto
 */
public interface ProcedureFluent<T> {

	/**
	 * 出力パラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param sqlType {@link java.sql.SQLType}で表されるSQLの型
	 * @return T
	 */
	T outParam(String paramName, SQLType sqlType);

	/**
	 * 出力パラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param sqlType {@link java.sql.Types}で表されるSQLの型
	 * @return T
	 */
	T outParam(String paramName, int sqlType);

	/**
	 * 入出力パラメータ追加<br>
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.SQLType}で表されるSQLの型
	 * @return T
	 */
	<V> T inOutParam(String paramName, V value, SQLType sqlType);

	/**
	 * 入出力パラメータ追加<br>
	 *
	 * 指定したパラメータ名がまだ登録されていない場合に値を追加する
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.SQLType}で表されるSQLの型
	 * @return T
	 */
	<V> T inOutParamIfAbsent(String paramName, V value, SQLType sqlType);

	/**
	 * 入出力パラメータ追加<br>
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types}で表されるSQLの型
	 * @return T
	 */
	<V> T inOutParam(String paramName, V value, int sqlType);

	/**
	 * 入出力パラメータ追加<br>
	 *
	 * 指定したパラメータ名がまだ登録されていない場合に値を追加する
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types}で表されるSQLの型
	 * @return T
	 */
	<V> T inOutParamIfAbsent(String paramName, V value, int sqlType);

}