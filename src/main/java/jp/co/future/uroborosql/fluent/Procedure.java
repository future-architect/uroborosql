/**
 *
 */
package jp.co.future.uroborosql.fluent;

import java.sql.SQLException;
import java.util.Map;

/**
 * Procedure 実行インタフェース
 *
 * @author H.Sugimoto
 */
public interface Procedure extends SqlFluent<Procedure> {
	/**
	 * Procedureの実行（終端処理）
	 *
	 * @return OUTパラメータのMap
	 * @throws SQLException SQL例外
	 */
	Map<String, Object> call() throws SQLException;
}
