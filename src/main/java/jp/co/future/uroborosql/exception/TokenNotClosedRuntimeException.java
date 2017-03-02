package jp.co.future.uroborosql.exception;

/**
 * SQLパース時、コメントブロックが閉じられていない場合にスローされる例外
 *
 * @author H.Sugimoto
 */
public class TokenNotClosedRuntimeException extends SqlParserRuntimeException {

	public TokenNotClosedRuntimeException(final String message) {
		super("コメントブロックが閉じられていません。[" + message + "]");
	}

}
