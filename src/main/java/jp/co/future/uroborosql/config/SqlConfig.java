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
	 * @return 生成したSqlAgent
	 */
	SqlAgent createAgent();

	/**
	 * sqlManager を取得します。
	 *
	 * @return sqlManager
	 */
	SqlManager getSqlManager();

	/**
	 * sqlFilterManager を取得します。
	 *
	 * @return sqlFilterManager
	 */
	SqlFilterManager getSqlFilterManager();

	/**
	 * connectionSupplier を取得します。
	 *
	 * @return connectionSupplier
	 */
	ConnectionSupplier getConnectionSupplier();

	/**
	 * sqlContextFactory を取得します。
	 *
	 * @return sqlContextFactory
	 */
	SqlContextFactory getSqlContextFactory();

	/**
	 * sqlAgentFactory を取得します。
	 *
	 * @return sqlAgentFactory
	 */
	SqlAgentFactory getSqlAgentFactory();

	/**
	 * dialect を取得します
	 *
	 * @return dialect
	 */
	Dialect getDialect();

	/**
	 * entityHandler を取得します
	 *
	 * @return entityHandler
	 */
	EntityHandler<?> getEntityHandler();

	/**
	 * entityHandler を設定します
	 *
	 * @param entityHandler
	 */
	void setEntityHandler(EntityHandler<?> entityHandler);
}