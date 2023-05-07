/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.SQLException;
import java.util.Objects;

@FunctionalInterface
public interface ExecutionConsumer<T> {
	/**
	 * 指定された引数でこのオペレーションを実行します.
	 *
	 * @param t 引数
	 * @throws SQLException SQL例外発生時
	 */
	void accept(T t) throws SQLException;

	/**
	 * このオペレーションを実行した後、続けてafterオペレーションを実行する合成TriConsumerを返します。
	 *
	 * @param after このオペレーションの後で実行するオペレーション
	 * @return このオペレーションを実行した後、続けてafterオペレーションを実行する合成BiConsumer
	 * @throws NullPointerException afterがnullの場合
	 */
	default ExecutionConsumer<T> andThen(final ExecutionConsumer<? super T> after) {
		Objects.requireNonNull(after);
		return t -> {
			accept(t);
			after.accept(t);
		};
	}

}
