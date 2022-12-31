/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.filter;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * Sql操作に対するフィルターインタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlFilter {
	/**
	 * 初期化メソッド
	 */
	void initialize();

	/**
	 * Parameterに対するフィルター処理を行う
	 *
	 * @param parameter 変換前パラメータ
	 * @return 変換後パラメータ
	 */
	Parameter doParameter(Parameter parameter);

	/**
	 * ストアドプロシージャのOutParameterに対するフィルター処理を行う
	 *
	 * @param key
	 *            パラメータ名
	 * @param val
	 *            OutParameterの値
	 * @return 変換後の値
	 */
	Object doOutParameter(String key, Object val);

	/**
	 * SQLを加工するフィルター処理を行う
	 * @param executionContext ExecutionContext
	 * @param sql 加工対象SQL文字列
	 * @return 加工後のSQL文字列
	 */
	String doTransformSql(ExecutionContext executionContext, String sql);

	/**
	 * PreparedStatementに対するフィルター処理を行う
	 *
	 * @param executionContext ExecutionContext
	 * @param preparedStatement 元のPreparedStatement
	 *
	 * @return フィルター後のPreparedStatement
	 * @throws SQLException SQL例外
	 */
	PreparedStatement doPreparedStatement(ExecutionContext executionContext, PreparedStatement preparedStatement)
			throws SQLException;

	/**
	 * CallableStatementに対するフィルター処理を行う
	 *
	 * @param executionContext ExecutionContext
	 * @param callableStatement 元のCallableStatement
	 *
	 * @return フィルター後のCallableStatement
	 * @throws SQLException SQL例外
	 */
	CallableStatement doCallableStatement(ExecutionContext executionContext, CallableStatement callableStatement)
			throws SQLException;

	/**
	 * 検索処理に対するフィルター処理を行う
	 *
	 * @param executionContext ExecutionContext
	 * @param preparedStatement 実行するPreparedStatement
	 * @param resultSet 実行結果
	 *
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	ResultSet doQuery(ExecutionContext executionContext, PreparedStatement preparedStatement, ResultSet resultSet)
			throws SQLException;

	/**
	 * 更新処理に対するフィルター処理を行う
	 *
	 * @param executionContext ExecutionContext
	 * @param preparedStatement 実行するPreparedStatement
	 * @param result 実行結果
	 *
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	int doUpdate(ExecutionContext executionContext, PreparedStatement preparedStatement, int result)
			throws SQLException;

	/**
	 * バッチ処理に対するフィルター処理を行う
	 *
	 * @param executionContext ExecutionContext
	 * @param preparedStatement 実行するPreparedStatement
	 * @param result 実行結果
	 *
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	int[] doBatch(ExecutionContext executionContext, PreparedStatement preparedStatement, int[] result)
			throws SQLException;

	/**
	 * CallableProcedure処理に対するフィルター処理を行う
	 *
	 * @param executionContext ExecutionContext
	 * @param callableStatement 実行するCallableStatement
	 * @param result 実行結果
	 *
	 * @return 実行結果
	 * @throws SQLException SQL例外
	 */
	boolean doProcedure(ExecutionContext executionContext, CallableStatement callableStatement, boolean result)
			throws SQLException;
}
