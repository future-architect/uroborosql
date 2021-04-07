/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.context;

import java.util.Map;

import jp.co.future.uroborosql.parameter.Parameter;

/**
 * Sqlに自動的にバインドするパラメータを生成するためのインタフェース
 *
 * @author H.Sugimoto
 *
 */
public interface AutoBindParameterCreator {

	/**
	 * ExecutionContextに自動的にバインドするパラメータのマップを取得する
	 *
	 * @return パラメータマップ（key:パラメータ名, value:パラメータ）
	 */
	Map<String, Parameter> getBindParameterMap();
}
