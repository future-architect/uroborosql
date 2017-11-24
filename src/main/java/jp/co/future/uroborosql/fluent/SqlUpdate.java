/**
 *
 */
package jp.co.future.uroborosql.fluent;

import java.util.Map;
import java.util.function.BiPredicate;
import java.util.stream.Stream;

import jp.co.future.uroborosql.context.SqlContext;

/**
 * SQL Update 実行インタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlUpdate extends SqlFluent<SqlUpdate> {
	/**
	 * これまでに追加されたパラメータ群をバッチパラメータに格納する
	 *
	 * @return SqlUpdate
	 */
	SqlUpdate addBatch();

	/**
	 * 一括更新用のパラメータをStream形式で設定する
	 *
	 * @param stream パラメータを格納したMapのStream
	 * @return SqlUpdate
	 */
	SqlUpdate paramStream(Stream<Map<String, Object>> stream);

	/**
	 * 一括更新用のバッチフレームの判定条件を設定する
	 * <pre>
	 * ex)
	 *  batchWhen((ctx, row) -&gt; ctx.batchCount() == 100)
	 * </pre>
	 *
	 * @param condition 判定条件
	 * @return SqlUpdate
	 */
	SqlUpdate batchWhen(BiPredicate<SqlContext, Map<String, Object>> condition);

	/**
	 * 更新結果の取得（終端処理）
	 *
	 * @return 更新件数
	 */
	int count();

	/**
	 * 一括更新結果の取得（終端処理）
	 *
	 * 事前にparamStreamが設定されている場合、Streamを使用した一括更新処理を行う。
	 * paramStreamが設定されていない場合は、addBatch()で登録したバッチパラメータを使った一括更新処理を行う。
	 *
	 * @return 更新件数の配列
	 */
	int[] batch();
}
