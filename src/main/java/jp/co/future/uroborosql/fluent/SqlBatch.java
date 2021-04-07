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

import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.BiPredicate;
import java.util.stream.Stream;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * SQL Batch Update 実行インタフェース
 *
 * @author H.Sugimoto
 * @since 0.5.0
 */
public interface SqlBatch extends SqlFluent<SqlBatch> {
	/**
	 * 一括更新用のパラメータをStream形式で設定する
	 *
	 * @param stream パラメータを格納したMapまたはBeanのStream
	 * @return SqlBatch
	 */
	SqlBatch paramStream(Stream<?> stream);

	/**
	 * 一括更新用のバッチフレームの判定条件を設定する
	 * <pre>
	 * ex)
	 *  by((ctx, row) -&gt; ctx.batchCount() == 100)
	 * </pre>
	 *
	 * @param condition 判定条件
	 * @return SqlBatch
	 */
	SqlBatch by(BiPredicate<ExecutionContext, Map<String, Object>> condition);

	/**
	 * バッチ実行時の動作を設定する.
	 * 初期設定は何もしない
	 *
	 * @param action バッチ実行時動作
	 * @return SqlBatch
	 */
	SqlBatch batchWhen(BiConsumer<SqlAgent, ExecutionContext> action);

	/**
	 * エラー発生時の動作を設定する.
	 * 初期設定は {@link UroborosqlRuntimeException} をスロー
	 *
	 * @param action エラー時動作
	 * @return SqlBatch
	 */
	SqlBatch errorWhen(TriConsumer<SqlAgent, ExecutionContext, Exception> action);

	/**
	 * 更新結果の取得（終端処理）
	 *
	 * @return 更新件数
	 */
	int count();
}
