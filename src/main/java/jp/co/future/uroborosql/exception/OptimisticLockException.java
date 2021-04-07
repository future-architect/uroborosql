/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * 楽観的排他制御の実行時例外
 *
 * @author hoshi
 */
public class OptimisticLockException extends UroborosqlRuntimeException {

	public OptimisticLockException(final ExecutionContext context) {
		super(String.format("An error occurred due to optimistic locking.\nExecuted SQL [\n%s]\nparams:%s",
				context.getExecutableSql(), context.formatParams()));
	}

	public OptimisticLockException(final String message) {
		super(message);
	}

	public OptimisticLockException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public OptimisticLockException(final Throwable cause) {
		super(cause);
	}

}
