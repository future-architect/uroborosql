/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
/**
 *
 */
package jp.co.future.uroborosql.fluent;

import java.sql.SQLType;
import java.util.Map;
import java.util.function.Supplier;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * Fluent API を使用してDBアクセスするためのインタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlFluent<T> {

	/**
	 * ExecutionContextの取得<br>
	 *
	 * @return ExecutionContext
	 */
	ExecutionContext context();

	/**
	 * 指定したパラメータ名のパラメータがすでに追加されているかどうかを判定する
	 *
	 * @param paramName パラメータ名
	 * @return パラメータが追加されていれば<code>true</code>. それ以外は<code>false</code>
	 */
	boolean hasParam(String paramName);

	/**
	 * パラメータの追加<br>
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @return T
	 */
	<V> T param(String paramName, V value);

	/**
	 * パラメータの追加<br>
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param supplier パラメータ値を提供するSupplier. supplierの戻り値が<code>null</code>の場合はパラメータを設定しない.
	 * @return T
	 */
	<V> T param(String paramName, Supplier<V> supplier);

	/**
	 * 型指定のパラメータ追加<br>
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.SQLType}で表されるSQLの型
	 * @return T
	 */
	<V> T param(String paramName, V value, SQLType sqlType);

	/**
	 * 型指定のパラメータ追加<br>
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types}で表されるSQLの型
	 * @return T
	 */
	<V> T param(String paramName, V value, int sqlType);

	/**
	 * パラメータの追加<br>
	 *
	 * 指定した値が<code>null</code>、空文字、空配列、空List以外の場合に値を追加する
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @return T
	 */
	<V> T paramIfNotEmpty(String paramName, V value);

	/**
	 * 型指定のパラメータ追加<br>
	 *
	 * 指定した値が<code>null</code>、空文字、空配列、空List以外の場合に値を追加する
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.SQLType}で表されるSQLの型
	 * @return T
	 */
	<V> T paramIfNotEmpty(String paramName, V value, SQLType sqlType);

	/**
	 * 型指定のパラメータ追加<br>
	 *
	 * 指定した値が<code>null</code>、空文字、空配列、空List以外の場合に値を追加する
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types}で表されるSQLの型
	 * @return T
	 */
	<V> T paramIfNotEmpty(String paramName, V value, int sqlType);

	/**
	 * パラメータの追加<br>
	 *
	 * 指定したパラメータ名がまだ登録されていない場合に値を追加する
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @return T
	 */
	<V> T paramIfAbsent(String paramName, V value);

	/**
	 * 型指定のパラメータ追加<br>
	 *
	 * 指定したパラメータ名がまだ登録されていない場合に値を追加する
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.SQLType}で表されるSQLの型
	 * @return T
	 */
	<V> T paramIfAbsent(String paramName, V value, SQLType sqlType);

	/**
	 * 型指定のパラメータ追加<br>
	 *
	 * 指定したパラメータ名がまだ登録されていない場合に値を追加する
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types}で表されるSQLの型
	 * @return T
	 */
	<V> T paramIfAbsent(String paramName, V value, int sqlType);

	/**
	 * 引数として渡されたMapの[key, value]のセットをパラメータに追加<br>
	 *
	 * @param paramMap パラメータのKey-Valueセット
	 * @return T
	 */
	T paramMap(Map<String, Object> paramMap);

	/**
	 * 引数として渡されたObjectのフィールド名と値のセットをパラメータに追加<br>
	 *
	 * @param <V> Beanの型
	 * @param bean パラメータとなるオブジェクト
	 * @return T
	 */
	<V> T paramBean(V bean);

	/**
	 * リトライ回数を設定する。 リトライ待機時間は0msが設定される
	 *
	 * @param count リトライ回数
	 * @return T
	 */
	T retry(int count);

	/**
	 * リトライ回数を設定する
	 *
	 * @param count リトライ回数
	 * @param waitTime リトライ待機時間（ms）
	 *
	 * @return T
	 */
	T retry(int count, int waitTime);

	/**
	 * 発行するSQLに付与するSQL_IDを設定する
	 *
	 * @param sqlId SQL_ID文字列
	 * @return T
	 */
	T sqlId(String sqlId);
}
