/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.fluent;

import java.io.InputStream;
import java.io.Reader;
import java.sql.SQLType;
import java.util.Map;
import java.util.function.Supplier;

/**
 * 抽出条件インタフェース
 * @author H.Sugimoto
 *
 * @param <T> Entity型を指定したSqlFluent型
 */
public interface ExtractionCondition<T> extends SqlFluent<T> {

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
	 * Where句に not equal 条件を追加する.
	 *
	 * @param <V> 値の型
	 * @param col bind column name
	 * @param value 値
	 * @return T
	 */
	<V> T notEqual(String col, V value);

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
	 * Where句に 素の文字列指定で 条件を追加する. 複数回呼び出した場合はそれぞれの条件をANDで結合する.
	 *
	 * @param rawString Where句に出力する条件式
	 * @param paramMap 条件式にバインドするパラメータ群
	 * @return T
	 */
	T where(CharSequence rawString, Map<String, Object> paramMap);

	// deprecated methods

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりに {@link ExtractionCondition#equal(String, Object)} を使用してください.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object)
	 */
	@Override
	@Deprecated
	<V> T param(final String paramName, final V value);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりに {@link ExtractionCondition#equal(String, Object)} を使用してください.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.util.function.Supplier)
	 */
	@Override
	@Deprecated
	<V> T param(final String paramName, final Supplier<V> supplier);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりに {@link ExtractionCondition#equal(String, Object)} を使用してください.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfAbsent(java.lang.String, java.lang.Object)
	 */
	@Override
	@Deprecated
	<V> T paramIfAbsent(final String paramName, final V value);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりに {@link ExtractionCondition#equal(String, Object)} を使用してください.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramMap(java.util.Map)
	 */
	@Override
	@Deprecated
	T paramMap(final Map<String, Object> paramMap);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりに {@link ExtractionCondition#equal(String, Object)} を使用してください.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramBean(java.lang.Object)
	 */
	@Override
	@Deprecated
	<V> T paramBean(final V bean);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりに {@link ExtractionCondition#equal(String, Object)} を使用してください.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	@Deprecated
	<V> T param(final String paramName, final V value, final SQLType sqlType);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりに {@link ExtractionCondition#equal(String, Object)} を使用してください.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfAbsent(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	@Deprecated
	<V> T paramIfAbsent(final String paramName, final V value, final SQLType sqlType);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりに {@link ExtractionCondition#equal(String, Object)} を使用してください.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object, int)
	 */
	@Override
	@Deprecated
	<V> T param(final String paramName, final V value, final int sqlType);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりに {@link ExtractionCondition#equal(String, Object)} を使用してください.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfAbsent(java.lang.String, java.lang.Object, int)
	 */
	@Override
	@Deprecated
	<V> T paramIfAbsent(final String paramName, final V value, final int sqlType);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりに {@link ExtractionCondition#equal(String, Object)} を使用してください.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParam(java.lang.String, java.io.InputStream)
	 */
	@Override
	@Deprecated
	T blobParam(final String paramName, final InputStream value);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりはありません.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParamIfAbsent(java.lang.String, java.io.InputStream)
	 */
	@Override
	@Deprecated
	T blobParamIfAbsent(final String paramName, final InputStream value);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりはありません.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParam(java.lang.String, java.io.InputStream, int)
	 */
	@Override
	@Deprecated
	T blobParam(final String paramName, final InputStream value, final int len);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりはありません.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParamIfAbsent(java.lang.String, java.io.InputStream, int)
	 */
	@Override
	@Deprecated
	T blobParamIfAbsent(final String paramName, final InputStream value, final int len);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりはありません.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParam(java.lang.String, java.io.Reader)
	 */
	@Override
	@Deprecated
	T clobParam(final String paramName, final Reader value);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりはありません.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParamIfAbsent(java.lang.String, java.io.Reader)
	 */
	@Override
	@Deprecated
	T clobParamIfAbsent(final String paramName, final Reader value);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりはありません.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParam(java.lang.String, java.io.Reader, int)
	 */
	@Override
	@Deprecated
	T clobParam(final String paramName, final Reader value, final int len);

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated 代わりはありません.
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParamIfAbsent(java.lang.String, java.io.Reader, int)
	 */
	@Override
	@Deprecated
	T clobParamIfAbsent(final String paramName, final Reader value, final int len);

}