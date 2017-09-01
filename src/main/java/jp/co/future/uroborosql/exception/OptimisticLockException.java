package jp.co.future.uroborosql.exception;

/**
 * 楽観的排他制御の実行時例外
 *
 * @author hoshi
 */
public class OptimisticLockException extends UroborosqlRuntimeException {

	public OptimisticLockException() {
		super();
	}

	public OptimisticLockException(final String message) {
		super(message);
	}

	public OptimisticLockException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public OptimisticLockException(final Throwable cause) {
		super(cause);
	}

}
