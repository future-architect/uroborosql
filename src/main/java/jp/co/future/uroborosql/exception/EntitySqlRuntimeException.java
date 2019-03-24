/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

import java.sql.SQLException;

import jp.co.future.uroborosql.enums.SqlKind;

/**
 * ORMの実行エラーでスローされる例外
 *
 * @author ota
 */
public class EntitySqlRuntimeException extends UroborosqlRuntimeException {
	private final SqlKind procKind;

	/**
	 * コンストラクタ
	 *
	 * @param procKind 処理種別
	 * @param cause SQLException
	 */
	public EntitySqlRuntimeException(final SqlKind procKind, final SQLException cause) {
		super(cause);
		this.procKind = procKind;
	}

	/**
	 * Exceptionの発生した処理種別
	 *
	 * @return 処理種別
	 */
	public SqlKind getProcKind() {
		return this.procKind;
	}

}
