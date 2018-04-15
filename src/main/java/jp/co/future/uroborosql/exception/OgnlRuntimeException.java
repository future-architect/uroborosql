/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
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
