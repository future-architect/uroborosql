/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

/**
 * SQLパース時、コメントブロックが閉じられていない場合にスローされる例外
 *
 * @author H.Sugimoto
 */
public class TokenNotClosedRuntimeException extends SqlParserRuntimeException {

	public TokenNotClosedRuntimeException(final String message) {
		super("Comment block is not closed.[" + message + "]");
	}

}
