/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.fluent;

import java.util.Objects;

/**
 * 3つの入力引数を受け取って結果を返さないオペレーションを表します。
 *
 * @author H.Sugimoto
 *
 * @param <T> オペレーションの第1引数の型
 * @param <U> オペレーションの第2引数の型
 * @param <V> オペレーションの第3引数の型
 */
@FunctionalInterface
public interface TriConsumer<T, U, V> {
	/**
	 * 指定された引数でこのオペレーションを実行します。
	 *
	 * @param t 第1入力引数
	 * @param u 第2入力引数
	 * @param v 第3入力引数
	 */
	void accept(T t, U u, V v);

	/**
	 * このオペレーションを実行した後、続けてafterオペレーションを実行する合成TriConsumerを返します。
	 *
	 * @param after このオペレーションの後で実行するオペレーション
	 * @return このオペレーションを実行した後、続けてafterオペレーションを実行する合成BiConsumer
	 * @exception NullPointerException afterがnullの場合
	 */
	default TriConsumer<T, U, V> andThen(final TriConsumer<? super T, ? super U, ? super V> after) {
		Objects.requireNonNull(after);
		return (t, u, v) -> {
			accept(t, u, v);
			after.accept(t, u, v);
		};
	}
}
