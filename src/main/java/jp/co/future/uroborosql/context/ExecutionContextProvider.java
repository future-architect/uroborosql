/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.context;

import java.util.List;
import java.util.Map;

import jp.co.future.uroborosql.config.SqlConfigAware;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;

/**
 * ExecutionContextProvider interfaces.
 *
 * @author H.Sugimoto
 *
 */
public interface ExecutionContextProvider extends SqlConfigAware {
	/**
	 * 初期化処理.
	 */
	void initialize();

	/**
	 * ExecutionContext生成メソッド.
	 *
	 * @return ExecutionContext
	 */
	ExecutionContext createExecutionContext();

	/**
	 * 定数パラメータプレフィックスを取得します。
	 *
	 * @return 定数パラメータプレフィックス
	 */
	String getConstParamPrefix();

	/**
	 * 定数パラメータプレフィックスを設定.
	 *
	 * @param constParamPrefix
	 *            定数パラメータプレフィックス
	 * @return ExecutionContextProvider
	 */
	ExecutionContextProvider setConstParamPrefix(String constParamPrefix);

	/**
	 * 定数クラス名（FQDN）を取得.
	 *
	 * @return 定数クラス名（FQDN）
	 */
	List<String> getConstantClassNames();

	/**
	 * 定数クラス名（FQDN）を設定.
	 *
	 * @param constantClassNames
	 *            定数クラス名（FQDN）
	 * @return ExecutionContextProvider
	 */
	ExecutionContextProvider setConstantClassNames(List<String> constantClassNames);

	/**
	 * Enum定数パッケージ名を取得.
	 *
	 * @return Enum定数パッケージ名
	 */
	List<String> getEnumConstantPackageNames();

	/**
	 * Enum定数パッケージ名（FQDN）を設定.
	 *
	 * @param enumConstantPackageNames Enum定数パッケージ名
	 * @return ExecutionContextProvider
	 */
	ExecutionContextProvider setEnumConstantPackageNames(List<String> enumConstantPackageNames);

	/**
	 * 定数クラスパラメータマップを取得.
	 *
	 * @return 定数クラスパラメータマップ
	 */
	Map<String, Parameter> getConstParameterMap();

	/**
	 * パラメータ変換クラス{@link BindParameterMapper}を追加.
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 * @return ExecutionContextProvider
	 */
	ExecutionContextProvider addBindParamMapper(BindParameterMapper<?> parameterMapper);

	/**
	 * パラメータ変換クラス{@link BindParameterMapper}をremove.
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 * @return ExecutionContextProvider
	 */
	ExecutionContextProvider removeBindParamMapper(BindParameterMapper<?> parameterMapper);

	/**
	 * ExecutionContextに設定するResultSetTypeの初期値を指定.
	 *
	 * @param resultSetType ResultSetTypeの初期値
	 * @return ExecutionContextProvider
	 */
	ExecutionContextProvider setDefaultResultSetType(int resultSetType);

	/**
	 * ExecutionContextに設定するResultSetConcurrencyの初期値を指定.
	 *
	 * @param resultSetConcurrency ResultSetConcurrencyの初期値
	 * @return ExecutionContextProvider
	 */
	ExecutionContextProvider setDefaultResultSetConcurrency(int resultSetConcurrency);

}
