/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

import java.sql.*;

/**
 * SQL実行時例外クラス
 *
 * @author hoshi
 */
public class UroborosqlSQLException extends UroborosqlRuntimeException {

	public UroborosqlSQLException() {
		super();
	}

	public UroborosqlSQLException(final Throwable cause) {
		super(cause);
	}

	public int getErrorCode() {
		return ((SQLException)getCause()).getErrorCode();
	}
}
