/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

/**
 * 検索結果が2件以上で先頭行を取得しようとした場合にスローされる例外クラス
 *
 * @author hoshi-k
 */
public class DataNonUniqueException extends UroborosqlRuntimeException {

	public DataNonUniqueException() {
	}

	public DataNonUniqueException(final String message) {
		super(message);
	}

	public DataNonUniqueException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public DataNonUniqueException(final Throwable cause) {
		super(cause);
	}

}
