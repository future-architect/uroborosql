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

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.SqlAgentFactory;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextFactory;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.expr.ExpressionParser;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.store.SqlManager;

/**
 * SQLを発行するための設定を管理するクラスのインタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlConfig {

	/**
	 * SqlContextの生成
	 *
	 * @return 生成したSqlContext
	 */
	SqlContext context();

	/**
	 * ファイル指定のSqlContextの生成
	 *
	 * @param sqlName SQLファイルのルートからの相対パス（ファイル拡張子なし）を指定
	 * @return 生成したSqlContext
	 */
	SqlContext contextFrom(String sqlName);

	/**
	 * SQL文を指定したSqlContextの生成
	 *
	 * @param sql SQL文の文字列
	 * @return 生成したSqlContext
	 */
	SqlContext contextWith(String sql);

	/**
	 * SqlAgentの生成
	 *
	 * @deprecated Instead, use the agent() method.
	 *
	 * @return 生成したSqlAgent
	 */
	@Deprecated()
	SqlAgent createAgent();

	/**
	 * SqlAgentの生成
	 *
	 * @return 生成したSqlAgent
	 */
	SqlAgent agent();

	/**
	 * sqlManager を取得.
	 *
	 * @return sqlManager
	 */
	SqlManager getSqlManager();

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
	 * sqlContextFactory を取得.
	 *
	 * @return sqlContextFactory
	 */
	SqlContextFactory getSqlContextFactory();

	/**
	 * sqlAgentFactory を取得.
	 *
	 * @return sqlAgentFactory
	 */
	SqlAgentFactory getSqlAgentFactory();

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