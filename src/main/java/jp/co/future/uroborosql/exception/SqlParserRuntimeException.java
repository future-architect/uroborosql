package jp.co.future.uroborosql.exception;

/**
 * SQLパース時の実行時例外
 *
 * @author H.Sugimoto
 */
public class SqlParserRuntimeException extends UroborosqlRuntimeException {

	public SqlParserRuntimeException() {
		super();
	}

	public SqlParserRuntimeException(final String message) {
		super(message);
	}

	public SqlParserRuntimeException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public SqlParserRuntimeException(final Throwable cause) {
		super(cause);
	}

}
