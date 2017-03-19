/**
 *
 */
package jp.co.future.uroborosql.fluent;

import java.io.InputStream;
import java.io.Reader;
import java.sql.SQLType;
import java.util.Map;

import jp.co.future.uroborosql.parameter.Parameter;

/**
 * Fluent API を使用してDBアクセスするためのインタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlFluent<T> {
	/**
	 * パラメータの追加<br>
	 *
	 * @param param パラメータ
	 * @return T
	 */
	T param(Parameter param);

	/**
	 * パラメータの追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value 値
	 * @return T
	 */
	T param(String paramName, Object value);

	/**
	 * パラメータ配列の追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value 配列として追加する
	 * @return T
	 */
	T paramList(String paramName, Object... value);

	/**
	 * 引数として渡されたMapの[key, value]のセットをパラメータに追加<br>
	 *
	 * @param paramMap パラメータのKeyValueセット
	 * @return T
	 */
	T paramMap(Map<String, Object> paramMap);

	/**
	 * 型指定のパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.SQLType}で表されるSQLの型
	 * @return T
	 */
	T param(String paramName, Object value, SQLType sqlType);

	/**
	 * 型指定のパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types}で表されるSQLの型
	 * @return T
	 */
	T param(String paramName, Object value, int sqlType);

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
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.SQLType}で表されるSQLの型
	 * @return T
	 */
	T inOutParam(String paramName, Object value, SQLType sqlType);

	/**
	 * 入出力パラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types}で表されるSQLの型
	 * @return T
	 */
	T inOutParam(String paramName, Object value, int sqlType);

	/**
	 * ストリームパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value ストリーム型の値
	 * @return T
	 */
	T binaryStreamParam(String paramName, InputStream value);

	/**
	 * ストリームパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value ストリーム型の値
	 * @param len 入力ストリームの長さ
	 * @return T
	 */
	T binaryStreamParam(String paramName, InputStream value, int len);

	/**
	 * ストリームパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value ストリーム型の値
	 * @return T
	 */
	T asciiStreamParam(String paramName, InputStream value);

	/**
	 * ストリームパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value ストリーム型の値
	 * @param len 入力ストリームの長さ
	 * @return T
	 */
	T asciiStreamParam(String paramName, InputStream value, int len);

	/**
	 * ストリームパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value リーダー型の値
	 * @return T
	 */
	T characterStreamParam(String paramName, Reader value);

	/**
	 * ストリームパラメータ追加<br>
	 *
	 * @param paramName パラメータ名
	 * @param value リーダー型の値
	 * @param len リーダーの長さ
	 * @return T
	 */
	T characterStreamParam(String paramName, Reader value, int len);
}
