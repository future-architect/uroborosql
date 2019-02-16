/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.fluent;

import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import jp.co.future.uroborosql.SqlAgent;

/**
 * Entity取得 SQL Query 実行インタフェース
 *
 * @param <E> Entity型
 * @author ota
 */
public interface SqlEntityQuery<E> extends SqlFluent<SqlEntityQuery<E>> {

	/**
	 * 検索結果の取得（終端処理）
	 *
	 * @return 検索結果のEntityリスト.
	 *
	 * @see SqlAgent#query(Class)
	 */
	List<E> collect();

	/**
	 * 検索結果の先頭行を取得（終端処理）
	 *
	 * @return 検索結果の先頭行をEntityに変換したもの.
	 */
	Optional<E> first();

	/**
	 * 検索結果をEntityのStreamとして取得（終端処理）
	 *
	 * @return 検索結果を順次取得するStream.
	 */
	Stream<E> stream();

	/**
	 * Where句に equal 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> equal(String col, Object value);

	/**
	 * Where句に not equal 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notEqual(String col, Object value);

	/**
	 * Where句に greater than 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> greaterThan(String col, Object value);

	/**
	 * Where句に less than 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> lessThan(String col, Object value);

	/**
	 * Where句に greater equal 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> greaterEqual(String col, Object value);

	/**
	 * Where句に less equal 条件を追加する
	 * @param col bind column name
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> lessEqual(String col, Object value);

	/**
	 * Where句に in 条件を追加する
	 * @param col bind column name
	 * @param values 値の配列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> in(String col, Object... values);

	/**
	 * Where句に in 条件を追加する
	 * @param col bind column name
	 * @param valueList 値の集合
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> in(String col, Iterable<?> valueList);

	/**
	 * Where句に not in 条件を追加する
	 * @param col bind column name
	 * @param values 値の配列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notIn(String col, Object... values);

	/**
	 * Where句に not in 条件を追加する
	 * @param col bind column name
	 * @param valueList 値の集合
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notIn(String col, Iterable<?> valueList);

	/**
	 * Where句に like 条件を追加する。検索文字列はエスケープしない
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> like(String col, CharSequence searchValue);

	/**
	 * Where句に前方一致条件を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> startsWith(String col, CharSequence searchValue);

	/**
	 * Where句に後方一致条件を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> endsWith(String col, CharSequence searchValue);

	/**
	 * Where句に 部分一致条件を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> contains(String col, CharSequence searchValue);

	/**
	 * Where句に not like 条件を追加する
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notLike(String col, CharSequence searchValue);

	/**
	 * Where句に前方一致条件の否定を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notStartsWith(String col, CharSequence searchValue);

	/**
	 * Where句に後方一致条件の否定を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notEndsWith(String col, CharSequence searchValue);

	/**
	 * Where句に 部分一致条件の否定を追加する。検索文字列は内部でエスケープされる。
	 * @param col bind column name
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notContains(String col, CharSequence searchValue);

	/**
	 * Where句に between 条件を追加する
	 * @param col bind column name
	 * @param fromValue 開始値
	 * @param toValue 終了値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> between(String col, Object fromValue, Object toValue);

	/**
	 * Where句に is null 条件を追加する
	 * @param col bind column name
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> isNull(String col);

	/**
	 * Where句に is not null 条件を追加する
	 * @param col bind column name
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> isNotNull(String col);

	/**
	 * Where句に 素の文字列指定で 条件を追加する
	 * @param rawString Where句に出力する条件式
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> where(CharSequence rawString);

	/**
	 * ソート条件を指定（昇順）
	 * @param cols ソート対象カラム名の配列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> orderByAsc(String... cols);

	/**
	 * ソート条件を指定（降順）
	 * @param cols ソート対象カラム名の配列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> orderByDesc(String... cols);

	/**
	 * 検索結果の行数制限を指定する
	 * @param limit 取得する行数
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> limit(long limit);

	/**
	 * 検索結果の開始行を指定する
	 * @param offset 取得開始行
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> offset(long offset);

}
