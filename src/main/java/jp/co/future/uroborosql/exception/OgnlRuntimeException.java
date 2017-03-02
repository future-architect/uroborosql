package jp.co.future.uroborosql.exception;

/**
 * OGNLの実行エラーでスローされる例外
 *
 * @author H.Sugimoto
 *
 */
public class OgnlRuntimeException extends SqlParserRuntimeException {

	public OgnlRuntimeException(final String message, final Throwable cause) {
		super(message, cause);
	}

}
