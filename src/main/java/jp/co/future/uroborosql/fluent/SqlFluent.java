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

import java.io.InputStream;
import java.io.Reader;
import java.sql.SQLType;
import java.util.Map;
import java.util.function.Function;

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
	 * @param function パラメータ値を取得するFunction. functionの戻り値が<code>null</code>の場合はパラメータを設定しない.
	 * @return T
	 */
	<V> T param(String paramName, Function<ExecutionContext, V> function);

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
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types}で表されるSQLの型
	 * @return T
	 */
	<V> T param(String paramName, V value, int sqlType);

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
	 * BLOBパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value ストリーム型の値
	 * @return T
	 */
	T blobParam(String paramName, InputStream value);

	/**
	 * BLOBパラメータ追加<br>
	 *
	 * 指定したパラメータ名がまだ登録されていない場合に値を追加する
	 *
	 * @param paramName パラメータ名
	 * @param value ストリーム型の値
	 * @return T
	 */
	T blobParamIfAbsent(String paramName, InputStream value);

	/**
	 * BLOBパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value ストリーム型の値
	 * @param len 入力ストリームの長さ
	 * @return T
	 */
	T blobParam(String paramName, InputStream value, int len);

	/**
	 * BLOBパラメータ追加<br>
	 *
	 * 指定したパラメータ名がまだ登録されていない場合に値を追加する
	 *
	 * @param paramName パラメータ名
	 * @param value ストリーム型の値
	 * @param len 入力ストリームの長さ
	 * @return T
	 */
	T blobParamIfAbsent(String paramName, InputStream value, int len);

	/**
	 * CLOBパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value リーダー型の値
	 * @return T
	 */
	T clobParam(String paramName, Reader value);

	/**
	 * CLOBパラメータ追加<br>
	 *
	 * 指定したパラメータ名がまだ登録されていない場合に値を追加する
	 *
	 * @param paramName パラメータ名
	 * @param value リーダー型の値
	 * @return T
	 */
	T clobParamIfAbsent(String paramName, Reader value);

	/**
	 * CLOBパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value リーダー型の値
	 * @param len リーダーの長さ
	 * @return T
	 */
	T clobParam(String paramName, Reader value, int len);

	/**
	 * CLOBパラメータ追加<br>
	 *
	 * 指定したパラメータ名がまだ登録されていない場合に値を追加する
	 *
	 * @param paramName パラメータ名
	 * @param value リーダー型の値
	 * @param len リーダーの長さ
	 * @return T
	 */
	T clobParamIfAbsent(String paramName, Reader value, int len);

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
