package jp.co.future.uroborosql.exception;

/**
 * 検索結果が０件で先頭行を取得しようとした場合にスローされる例外クラス
 *
 * @author H.Sugimoto
 */
public class DataNotFoundException extends Exception {

	public DataNotFoundException() {
		super();
	}

	public DataNotFoundException(final String message) {
		super(message);
	}

	public DataNotFoundException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public DataNotFoundException(final Throwable cause) {
		super(cause);
	}

}
