package jp.co.future.uroborosql.exception;

/**
 * SQLパース時、終了コメントが見つからなかった場合にスローされる例外
 *
 * @author H.Sugimoto
 */
public class EndCommentNotFoundRuntimeException extends SqlParserRuntimeException {

	public EndCommentNotFoundRuntimeException() {
		super("END Comment not found.");
	}

}
