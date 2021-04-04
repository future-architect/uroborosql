/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
/**
 *
 */
package jp.co.future.uroborosql.config;

import java.time.Clock;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.SqlAgentProvider;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.context.ExecutionContextProvider;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.expr.ExpressionParser;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.store.SqlResourceManager;

/**
 * SQLを発行するための設定を管理するクラスのインタフェース.
 *
 * @author H.Sugimoto
 */
public interface SqlConfig {

	/**
	 * ExecutionContextの生成.
	 *
	 * @return 生成したExecutionContext
	 */
	ExecutionContext context();

	/**
	 * ファイル指定のExecutionContextの生成.
	 *
	 * @param sqlName SQLファイルのルートからの相対パス（ファイル拡張子なし）を指定
	 * @return 生成したExecutionContext
	 */
	ExecutionContext contextFrom(String sqlName);

	/**
	 * SQL文を指定したExecutionContextの生成.
	 *
	 * @param sql SQL文の文字列
	 * @return 生成したExecutionContext
	 */
	ExecutionContext contextWith(String sql);

	/**
	 * SqlAgentの生成.
	 *
	 * @return 生成したSqlAgent.
	 */
	SqlAgent agent();

	/**
	 * SqlAgentの生成.
	 *
	 * @param ctx DB接続情報
	 * @return 生成したSqlAgent.
	 */
	SqlAgent agent(ConnectionContext ctx);

	/**
	 * sqlResourceManager を取得.
	 *
	 * @return sqlResourceManager
	 */
	SqlResourceManager getSqlResourceManager();

	/**
	 * sqlFilterManager を取得.
	 *
	 * @return sqlFilterManager
	 */
	SqlFilterManager getSqlFilterManager();

	/**
	 * connectionSupplier を取得.
	 *
	 * @return connectionSupplier
	 */
	ConnectionSupplier getConnectionSupplier();

	/**
	 * executionContextProvider を取得.
	 *
	 * @return executionContextProvider
	 */
	ExecutionContextProvider getExecutionContextProvider();

	/**
	 * sqlAgentProvider を取得.
	 *
	 * @return sqlAgentProvider
	 */
	SqlAgentProvider getSqlAgentProvider();

	/**
	 * clock を取得.
	 *
	 * @return clock
	 */
	Clock getClock();

	/**
	 * dialect を取得.
	 *
	 * @return dialect
	 */
	Dialect getDialect();

	/**
	 * expressionParser を取得.
	 *
	 * @return expressionParser
	 */
	ExpressionParser getExpressionParser();

	/**
	 * entityHandler を取得.
	 *
	 * @return entityHandler
	 */
	EntityHandler<?> getEntityHandler();
}