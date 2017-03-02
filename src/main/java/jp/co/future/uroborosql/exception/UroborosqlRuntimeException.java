package jp.co.future.uroborosql.exception;

/**
 * ライブラリ実行時例外クラス
 *
 * @author H.Sugimoto
 */
public class UroborosqlRuntimeException extends RuntimeException {

	public UroborosqlRuntimeException() {
		super();
	}

	public UroborosqlRuntimeException(final String message) {
		super(message);
	}

	public UroborosqlRuntimeException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public UroborosqlRuntimeException(final Throwable cause) {
		super(cause);
	}

}
