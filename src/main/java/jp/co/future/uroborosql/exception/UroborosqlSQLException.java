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
