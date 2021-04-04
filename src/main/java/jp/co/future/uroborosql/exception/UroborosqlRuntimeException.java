/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

/**
 * ライブラリ実行時例外クラス
 *
 * @author H.Sugimoto
 */
public class UroborosqlRuntimeException extends RuntimeException {

	public UroborosqlRuntimeException() {
	}

	public UroborosqlRuntimeException(final String message) {
		super(message);
	}

	public UroborosqlRuntimeException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public UroborosqlRuntimeException(final Throwable cause) {
		super(cause);
	}

}
