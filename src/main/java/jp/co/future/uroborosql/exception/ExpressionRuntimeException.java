/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

/**
 * EL式の評価でエラーになった場合にスローされる例外.
 *
 * @author H.Sugimoto
 */
public class ExpressionRuntimeException extends RuntimeException {

	/**
	 * コンストラクタ
	 */
	public ExpressionRuntimeException() {
	}

	/**
	 * コンストラクタ
	 *
	 * @param message 例外メッセージ
	 */
	public ExpressionRuntimeException(final String message) {
		super(message);
	}

	/**
	 * コンストラクタ
	 *
	 * @param cause 元となる例外
	 */
	public ExpressionRuntimeException(final Throwable cause) {
		super(cause);
	}

	/**
	 * コンストラクタ
	 *
	 * @param message 例外メッセージ
	 * @param cause 元となる例外
	 */
	public ExpressionRuntimeException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
