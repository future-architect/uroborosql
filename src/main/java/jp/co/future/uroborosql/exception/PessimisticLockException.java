/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

import jp.co.future.uroborosql.context.SqlContext;

/**
 * 悲観的排他制御の実行時例外
 *
 * @author H.Sugimoto
 */
public class PessimisticLockException extends UroborosqlRuntimeException {

	public PessimisticLockException(final SqlContext context) {
		super(createMessage(context));
	}

	public PessimisticLockException(final SqlContext context, final Throwable cause) {
		super(createMessage(context), cause);
	}

	public PessimisticLockException(final String message) {
		super(message);
	}

	public PessimisticLockException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public PessimisticLockException(final Throwable cause) {
		super(cause);
	}

	private static String createMessage(final SqlContext context) {
		return String.format("An error occurred due to pessimistic locking.%nExecuted SQL [%n%s]\nparams:%s",
				context.getExecutableSql(), context.formatParams());
	}

}
