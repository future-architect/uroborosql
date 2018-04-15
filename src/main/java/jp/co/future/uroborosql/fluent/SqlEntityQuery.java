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
}
