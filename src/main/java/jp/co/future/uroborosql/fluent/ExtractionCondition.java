/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.fluent;

import java.util.Map;

/**
 * 抽出条件インタフェース
 * @author H.Sugimoto
 *
 * @param <T> Entity型を指定したSqlFluent型
 */
public interface ExtractionCondition<T> {

	/**
	 * Where句に equal 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	<V> T equal(String col, V value);

	/**
	 * 値が空でない場合にWhere句に equal 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	<V> T equalIfNotEmpty(String col, V value);

	/**
	 * Where句に not equal 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	<V> T notEqual(String col, V value);

	/**
	 * 値が空でない場合にWhere句に not equal 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	<V> T notEqualIfNotEmpty(String col, V value);

	/**
	 * Where句に greater than 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	<V> T greaterThan(String col, V value);

	/**
	 * Where句に less than 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	<V> T lessThan(String col, V value);

	/**
	 * Where句に greater equal 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	<V> T greaterEqual(String col, V value);

	/**
	 * Where句に less equal 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	<V> T lessEqual(String col, V value);

	/**
	 * Where句に in 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param values 値の配列
	 * @return T
	 */
	<V> T in(String col, @SuppressWarnings("unchecked") V... values);

	/**
	 * Where句に in 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param valueList 値の集合
	 * @return T
	 */
	<V> T in(String col, Iterable<V> valueList);

	/**
	 * Where句に not in 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param values 値の配列
	 * @return T
	 */
	<V> T notIn(String col, @SuppressWarnings("unchecked") V... values);

	/**
	 * Where句に not in 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param valueList 値の集合
	 * @return T
	 */
	<V> T notIn(String col, Iterable<V> valueList);

	/**
	 * Where句に like 条件を追加する. 検索文字列はエスケープしない.
	 *
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T like(String col, CharSequence searchValue);

	/**
	 * Where句に前方一致条件を追加する. 検索文字列は内部でエスケープされる.
	 *
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T startsWith(String col, CharSequence searchValue);

	/**
	 * Where句に後方一致条件を追加する. 検索文字列は内部でエスケープされる.
	 *
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T endsWith(String col, CharSequence searchValue);

	/**
	 * Where句に 部分一致条件を追加する. 検索文字列は内部でエスケープされる.
	 *
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T contains(String col, CharSequence searchValue);

	/**
	 * Where句に not like 条件を追加する.
	 *
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T notLike(String col, CharSequence searchValue);

	/**
	 * Where句に前方一致条件の否定を追加する. 検索文字列は内部でエスケープされる.
	 *
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T notStartsWith(String col, CharSequence searchValue);

	/**
	 * Where句に後方一致条件の否定を追加する. 検索文字列は内部でエスケープされる.
	 *
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T notEndsWith(String col, CharSequence searchValue);

	/**
	 * Where句に 部分一致条件の否定を追加する. 検索文字列は内部でエスケープされる.
	 *
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T notContains(String col, CharSequence searchValue);

	/**
	 * Where句に between 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param fromValue 開始値
	 * @param toValue 終了値
	 * @return T
	 */
	<V> T between(String col, V fromValue, V toValue);

	/**
	 * Where句に not between 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param fromValue 開始値
	 * @param toValue 終了値
	 * @return T
	 */
	<V> T notBetween(String col, V fromValue, V toValue);

	/**
	 * Where句に between 条件を追加する.(値に対してfrom/toのカラムを指定する場合)
	 *
	 * @param <V> 値の型
	 * @param value betweenの評価対象となる値
	 * @param fromCol 開始値となるカラムの名前
	 * @param toCol 終了値となるカラムの名前
	 * @return T
	 */
	<V> T betweenColumns(V value, String fromCol, String toCol);

	/**
	 * Where句に not between 条件を追加する.(値に対してfrom/toのカラムを指定する場合)
	 *
	 * @param <V> 値の型
	 * @param value betweenの評価対象となる値
	 * @param fromCol 開始値となるカラムの名前
	 * @param toCol 終了値となるカラムの名前
	 * @return T
	 */
	<V> T notBetweenColumns(V value, String fromCol, String toCol);

	/**
	 * Where句に is null 条件を追加する.
	 *
	 * @param col bind column name
	 * @return T
	 */
	T isNull(String col);

	/**
	 * Where句に is not null 条件を追加する.
	 *
	 * @param col bind column name
	 * @return T
	 */
	T isNotNull(String col);

	/**
	 * Where句に 素の文字列指定で 条件を追加する. 複数回呼び出した場合はそれぞれの条件をANDで結合する.
	 *
	 * @param rawString Where句に出力する条件式
	 * @return T
	 */
	T where(CharSequence rawString);

	/**
	 * Where句に 素の文字列指定で 条件を追加する. 複数回呼び出した場合はそれぞれの条件をANDで結合する.
	 *
	 * @param <V> 値の型
	 * @param rawString Where句に出力する条件式
	 * @param paramName 条件式にバインドするパラメータ名
	 * @param value 条件式にバインドする値
	 * @return T
	 */
	<V> T where(CharSequence rawString, String paramName, V value);

	/**
	 * 条件式にバインドする値が空でない場合にWhere句に 素の文字列指定で 条件を追加する. 複数回呼び出した場合はそれぞれの条件をANDで結合する.
	 *
	 * @param <V> 値の型
	 * @param rawString Where句に出力する条件式
	 * @param paramName 条件式にバインドするパラメータ名
	 * @param value 条件式にバインドする値
	 * @return T
	 */
	<V> T whereIfNotEmpty(CharSequence rawString, String paramName, V value);

	/**
	 * Where句に 素の文字列指定で 条件を追加する. 複数回呼び出した場合はそれぞれの条件をANDで結合する.
	 *
	 * @param rawString Where句に出力する条件式
	 * @param paramMap 条件式にバインドするパラメータ群
	 * @return T
	 */
	T where(CharSequence rawString, Map<String, Object> paramMap);

	/**
	 * ExecutionContextが保持する属性を取得する
	 * @return ExecutionContext属性情報
	 */
	Map<String, Object> contextAttrs();

}