package jp.co.future.uroborosql.exception;

/**
 *
 * SQLパース時、IF条件が見つからなかった場合にスローされる例外
 *
 * @author H.Sugimoto
 */
public class IfConditionNotFoundRuntimeException extends RuntimeException {

	public IfConditionNotFoundRuntimeException() {
		super("IF condition is not specified.");
	}

}
