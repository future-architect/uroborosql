package jp.co.future.uroborosql.exception;

/**
 * SQLパース時、IF条件がboolean型を返さなかった場合にスローされる例外
 *
 * @author H.Sugimoto
 *
 */
public class IllegalBoolExpressionRuntimeException extends RuntimeException {

	public IllegalBoolExpressionRuntimeException(final String message) {
		super("boolean型の値が取得できませんでした。[" + message + "]");
	}

}
