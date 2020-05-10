/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import java.time.Clock;

/**
 * パラメータをJDBCが受け入れられる型に変換するインターフェース. 日付変換を行うためにClockを使用する.
 *
 * @param <T> 変換対象の型
 *
 * @author H.Sugimoto
 */
public interface BindParameterMapperWithClock<T> extends BindParameterMapper<T> {
	/**
	 * Clock の取得
	 * @return Clock
	 */
	Clock getClock();

	/**
	 * Clock の設定
	 * @param clock Clock
	 */
	void setClock(Clock clock);
}
