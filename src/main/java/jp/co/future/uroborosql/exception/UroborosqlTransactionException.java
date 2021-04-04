/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

/**
 * トランザクションに関連する実行時例外クラス
 *
 * @author H.Sugimoto
 * @since v0.14.0
 */
public class UroborosqlTransactionException extends UroborosqlRuntimeException {

	public UroborosqlTransactionException() {
	}

	public UroborosqlTransactionException(final String message) {
		super(message);
	}

	public UroborosqlTransactionException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public UroborosqlTransactionException(final Throwable cause) {
		super(cause);
	}

}
