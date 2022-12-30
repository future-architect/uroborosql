/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

/**
 * SQLパース時の実行時例外
 *
 * @author H.Sugimoto
 */
public class SqlParserRuntimeException extends UroborosqlRuntimeException {

	public SqlParserRuntimeException() {
		super();
	}

	public SqlParserRuntimeException(final String message) {
		super(message);
	}

	public SqlParserRuntimeException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public SqlParserRuntimeException(final Throwable cause) {
		super(cause);
	}

}
