package jp.co.future.uroborosql.exception;

/**
 * SQLパース時、パラメータが取得できなかった場合にスローされる例外
 *
 * @author H.Sugimoto
 */
public class ParameterNotFoundRuntimeException extends UroborosqlRuntimeException {

	public ParameterNotFoundRuntimeException(final String message) {
		super(message);
	}

}
