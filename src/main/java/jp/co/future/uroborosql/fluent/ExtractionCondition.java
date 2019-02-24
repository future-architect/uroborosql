/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.fluent;

/**
 * 抽出条件インタフェース
 * @author H.Sugimoto
 *
 * @param <T> Entity型を指定したSqlFluent型
 */
public interface ExtractionCondition<T> extends SqlFluent<T> {

	/**
	 * Where句に equal 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	T equal(String col, Object value);

	/**
	 * Where句に not equal 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	T notEqual(String col, Object value);

	/**
	 * Where句に greater than 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	T greaterThan(String col, Object value);

	/**
	 * Where句に less than 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	T lessThan(String col, Object value);

	/**
	 * Where句に greater equal 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	T greaterEqual(String col, Object value);

	/**
	 * Where句に less equal 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	T lessEqual(String col, Object value);

	/**
	 * Where句に in 条件を追加する
	 * @param col bind column name
	 * @param values 値の配列
	 * @return T
	 */
	T in(String col, Object... values);

	/**
	 * Where句に in 条件を追加する
	 * @param col bind column name
	 * @param valueList 値の集合
	 * @return T
	 */
	T in(String col, Iterable<?> valueList);

	/**
	 * Where句に not in 条件を追加する
	 * @param col bind column name
	 * @param values 値の配列
	 * @return T
	 */
	T notIn(String col, Object... values);

	/**
	 * Where句に not in 条件を追加する
	 * @param col bind column name
	 * @param valueList 値の集合
	 * @return T
	 */
	T notIn(String col, Iterable<?> valueList);

	/**
	 * Where句に like 条件を追加する。検索文字列はエスケープしない
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T like(String col, CharSequence searchValue);

	/**
	 * Where句に前方一致条件を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T startsWith(String col, CharSequence searchValue);

	/**
	 * Where句に後方一致条件を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T endsWith(String col, CharSequence searchValue);

	/**
	 * Where句に 部分一致条件を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T contains(String col, CharSequence searchValue);

	/**
	 * Where句に not like 条件を追加する
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T notLike(String col, CharSequence searchValue);

	/**
	 * Where句に前方一致条件の否定を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T notStartsWith(String col, CharSequence searchValue);

	/**
	 * Where句に後方一致条件の否定を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T notEndsWith(String col, CharSequence searchValue);

	/**
	 * Where句に 部分一致条件の否定を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return T
	 */
	T notContains(String col, CharSequence searchValue);

	/**
	 * Where句に between 条件を追加する
	 * @param col bind column name
	 * @param fromValue 開始値
	 * @param toValue 終了値
	 * @return T
	 */
	T between(String col, Object fromValue, Object toValue);

	/**
	 * Where句に is null 条件を追加する
	 * @param col bind column name
	 * @return T
	 */
	T isNull(String col);

	/**
	 * Where句に is not null 条件を追加する
	 * @param col bind column name
	 * @return T
	 */
	T isNotNull(String col);

	/**
	 * Where句に 素の文字列指定で 条件を追加する。複数回呼び出した場合はそれぞれの条件をANDで結合する。
	 * @param rawString Where句に出力する条件式
	 * @return T
	 */
	T where(CharSequence rawString);

}