/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

/**
 * 検索結果が０件で先頭行を取得しようとした場合にスローされる例外クラス
 *
 * @author H.Sugimoto
 */
public class DataNotFoundException extends UroborosqlRuntimeException {

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
