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
import jp.co.future.uroborosql.SqlAgentFactory;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * Entity取得 SQL Query 実行インタフェース
 *
 * @param <E> Entity型
 * @author ota
 */
public interface SqlEntityQuery<E> extends ExtractionCondition<SqlEntityQuery<E>> {
	/**
	 * ORDER BY句の順序を表すEnum
	 */
	public enum Order {
		ASCENDING("ASC"), DESCENDING("DESC");

		private String alias;

		Order(final String alias) {
			this.alias = alias;
		}

		@Override
		public String toString() {
			return alias;
		}
	}

	/**
	 * NULLS FIRST/LASTを表現するEnum
	 */
	public enum Nulls {
		FIRST, LAST;

		@Override
		public String toString() {
			return "NULLS " + name();
		}
	}

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
	 * 検索結果の件数を取得（終端処理）
	 *
	 * @return 検索結果件数.
	 */
	long count();

	/**
	 * 検索結果のうち、引数で指定したカラムがNULLでない行の件数を取得（終端処理）
	 *
	 * @param col count target column name
	 * @return 引数で指定したカラムがNULLでない行の件数.
	 */
	long count(String col);

	/**
	 * 検索結果のうち、引数で指定したカラムの合計値を取得（終端処理）
	 *
	 * @param <T> return value type
	 * @param col sum target column name
	 * @return 合計値.
	 */
	<T> T sum(String col);

	/**
	 * 検索結果のうち、引数で指定したカラムの最小値を取得（終端処理）
	 *
	 * @param <T> return value type
	 * @param col min target column name
	 * @return 最小値.
	 */
	<T> T min(String col);

	/**
	 * 検索結果が1件以上ある場合にRunnableを実行する
	 *
	 * @param runnable 検索結果が1件以上ある場合に実行するRunnable
	 */
	void exists(Runnable runnable);

	/**
	 * 検索結果が0件の場合にRunnableを実行する
	 *
	 * @param runnable 検索結果が0件の場合に実行するRunnable
	 */
	void notExists(Runnable runnable);

	/**
	 * 検索結果のうち、引数で指定したカラムの最大値を取得（終端処理）
	 *
	 * @param <T> return value type
	 * @param col max target column name
	 * @return 最大値.
	 */
	<T> T max(String col);

	/**
	 * ソート条件を指定（昇順）
	 * @param cols sort target column names
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> asc(String... cols);

	/**
	 * ソート条件を指定（昇順）
	 * @param col sort target column name
	 * @param nulls {@link Nulls}
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> asc(String col, Nulls nulls);

	/**
	 * ソート条件を指定（降順）
	 * @param cols sort target column names
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> desc(String... cols);

	/**
	 * ソート条件を指定（降順）
	 * @param col sort target column name
	 * @param nulls {@link Nulls}
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> desc(String col, Nulls nulls);

	/**
	 * 検索結果の行数制限を指定する。limitがサポートされていないデータベースの場合は {@link UroborosqlRuntimeException} がスローされる
	 * @param limit 取得する行数
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> limit(long limit);

	/**
	 * 検索結果の開始行を指定する。offsetがサポートされていないデータベースの場合は {@link UroborosqlRuntimeException} がスローされる
	 * @param offset 取得開始行
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> offset(long offset);

	/**
	 * 明示的な行ロックを行う
	 *
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> forUpdate();

	/**
	 * 明示的な行ロックを行う（待機なし）
	 *
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> forUpdateNoWait();

	/**
	 * 明示的な行ロックを行う（待機時間指定）<br>
	 * 待機時間は{@link SqlAgentFactory#setDefaultForUpdateWaitSeconds(int)}で設定した値を使用する
	 *
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> forUpdateWait();

	/**
	 * 明示的な行ロックを行う（待機時間指定）
	 *
	 * @param waitSeconds 待機時間（秒）
	 * @return SqlEntityQuery
	 */
	SqlEntityQuery<E> forUpdateWait(int waitSeconds);

}
