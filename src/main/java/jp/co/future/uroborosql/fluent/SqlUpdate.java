/**
 *
 */
package jp.co.future.uroborosql.fluent;

import java.sql.SQLException;

/**
 * SQL Update 実行インタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlUpdate extends SqlFluent<SqlUpdate> {
	/**
	 * 更新結果の取得（終端処理）
	 *
	 * @return 更新件数
	 * @throws SQLException SQL例外
	 */
	int count() throws SQLException;
}
