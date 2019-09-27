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

import java.sql.SQLException;
import java.util.Map;

/**
 * Procedure 実行インタフェース
 *
 * @author H.Sugimoto
 */
public interface Procedure extends SqlFluent<Procedure>, ProcedureFluent<Procedure> {
	/**
	 * Procedureの実行（終端処理）
	 *
	 * @return OUTパラメータのMap
	 * @throws SQLException SQL例外
	 */
	Map<String, Object> call() throws SQLException;
}
