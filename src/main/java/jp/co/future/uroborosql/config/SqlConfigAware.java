package jp.co.future.uroborosql.config;

/**
 * SqlConfigに対するアクセスインタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlConfigAware {

	/**
	 * SqlConfigの設定.
	 *
	 * @param sqlConfig SqlConfig
	 */
	void setSqlConfig(SqlConfig sqlConfig);

	/**
	 * SqlConfigの取得.
	 *
	 * @return SqlConfig
	 */
	SqlConfig getSqlConfig();
}
