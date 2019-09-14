package jp.co.future.uroborosql.exception;

/**
 * トランザクションに関連する実行時例外クラス
 *
 * @author H.Sugimoto
 * @since v0.14.0
 */
public class UroborosqlTransactionException extends UroborosqlRuntimeException {

	public UroborosqlTransactionException() {
		super();
	}

	public UroborosqlTransactionException(final String message) {
		super(message);
	}

	public UroborosqlTransactionException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public UroborosqlTransactionException(final Throwable cause) {
		super(cause);
	}

}
