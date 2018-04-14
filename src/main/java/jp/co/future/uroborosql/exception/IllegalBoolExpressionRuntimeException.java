/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

/**
 * SQLパース時、IF条件がboolean型を返さなかった場合にスローされる例外
 *
 * @author H.Sugimoto
 *
 */
public class IllegalBoolExpressionRuntimeException extends RuntimeException {

	public IllegalBoolExpressionRuntimeException(final String message) {
		super("A boolean type value could not be obtained.[" + message + "]");
	}

}
