/**
 *
 */
package jp.co.future.uroborosql.fluent;

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
	 * 更新結果の取得（終端処理）
	 *
	 * @return 更新件数
	 */
	int count();

	/**
	 * 一括更新結果の取得（終端処理）
	 *
	 * @return 更新件数の配列
	 */
	int[] batch();
}
