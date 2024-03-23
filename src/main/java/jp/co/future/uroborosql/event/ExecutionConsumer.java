/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.SQLException;

/**
 * SQLやProcedureを実行する際に利用するConsumer.<br>
 * SQLExceptionをスローするため、{@link java.util.function.Consumer} とは別に作成する.
 *
 * @param <T> the type of the input to the operation
 */
@FunctionalInterface
public interface ExecutionConsumer<T> {
	/**
	 * 指定された引数でこのオペレーションを実行します.
	 *
	 * @param t 引数
	 * @throws SQLException SQL例外発生時
	 */
	void accept(T t) throws SQLException;
}
