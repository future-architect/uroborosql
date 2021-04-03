/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

import java.sql.SQLException;

/**
 * SQL実行時例外クラス
 *
 * @author hoshi
 */
public class UroborosqlSQLException extends UroborosqlRuntimeException {

	public UroborosqlSQLException() {
	}

	public UroborosqlSQLException(final Throwable cause) {
		super(cause);
	}

	public UroborosqlSQLException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public int getErrorCode() {
		return getCause() instanceof SQLException ? ((SQLException) getCause()).getErrorCode() : 0;
	}

	public String getSQLState() {
		return getCause() instanceof SQLException ? ((SQLException) getCause()).getSQLState() : null;
	}
}
