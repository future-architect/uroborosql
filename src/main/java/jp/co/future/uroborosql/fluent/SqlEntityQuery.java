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
	 * @param col 対象カラム
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> equal(String col, Object value);

	/**
	 * Where句に not equal 条件を追加する
	 * @param col 対象カラム
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notEqual(String col, Object value);

	/**
	 * Where句に greater than 条件を追加する
	 * @param col 対象カラム
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> greaterThan(String col, Object value);

	/**
	 * Where句に less than 条件を追加する
	 * @param col 対象カラム
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> lessThan(String col, Object value);

	/**
	 * Where句に greater equal 条件を追加する
	 * @param col 対象カラム
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> greaterEqual(String col, Object value);

	/**
	 * Where句に less equal 条件を追加する
	 * @param col 対象カラム
	 * @param value 値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> lessEqual(String col, Object value);

	/**
	 * Where句に in 条件を追加する
	 * @param col 対象カラム
	 * @param values 値の配列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> in(String col, Object... values);

	/**
	 * Where句に in 条件を追加する
	 * @param col 対象カラム
	 * @param valueList 値のList
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> in(String col, List<Object> valueList);

	/**
	 * Where句に not in 条件を追加する
	 * @param col 対象カラム
	 * @param values 値の配列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notIn(String col, Object... values);

	/**
	 * Where句に not in 条件を追加する
	 * @param col 対象カラム
	 * @param valueList 値のList
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notIn(String col, List<Object> valueList);

	/**
	 * Where句に like 条件を追加する
	 * @param col 対象カラム
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> like(String col, String searchValue);

	/**
	 * Where句に like 条件を追加する
	 * @param col 対象カラム
	 * @param prefix 検索文字列の前に%を付ける
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> like(String col, boolean prefix, String searchValue);

	/**
	 * Where句に like 条件を追加する
	 * @param col 対象カラム
	 * @param searchValue 検索文字列
	 * @param suffix 検索文字列の後に%を付ける
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> like(String col, String searchValue, boolean suffix);

	/**
	 * Where句に like 条件を追加する
	 * @param col 対象カラム
	 * @param prefix 検索文字列の前に%を付ける
	 * @param searchValue 検索文字列
	 * @param suffix 検索文字列の後に%を付ける
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> like(String col, boolean prefix, String searchValue, boolean suffix);

	/**
	 * Where句に not like 条件を追加する
	 * @param col 対象カラム
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notLike(String col, String searchValue);

	/**
	 * Where句に not like 条件を追加する
	 * @param col 対象カラム
	 * @param prefix 検索文字列の前に%を付ける
	 * @param searchValue 検索文字列
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notLike(String col, boolean prefix, String searchValue);

	/**
	 * Where句に not like 条件を追加する
	 * @param col 対象カラム
	 * @param searchValue 検索文字列
	 * @param suffix 検索文字列の後に%を付ける
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notLike(String col, String searchValue, boolean suffix);

	/**
	 * Where句に not like 条件を追加する
	 * @param col 対象カラム
	 * @param prefix 検索文字列の前に%を付ける
	 * @param searchValue 検索文字列
	 * @param suffix 検索文字列の後に%を付ける
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> notLike(String col, boolean prefix, String searchValue, boolean suffix);

	/**
	 * Where句に between 条件を追加する
	 * @param col 対象カラム
	 * @param fromValue 開始値
	 * @param toValue 終了値
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> between(String col, Object fromValue, Object toValue);

	/**
	 * Where句に is null 条件を追加する
	 * @param col 対象カラム
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> isNull(String col);

	/**
	 * Where句に is not null 条件を追加する
	 * @param col 対象カラム
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> isNotNull(String col);

	/**
	 * Where句に 素の文字列指定で 条件を追加する
	 * @param col 対象カラム
	 * @param rawString Where句に出力する条件式
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> where(String rawString);

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
