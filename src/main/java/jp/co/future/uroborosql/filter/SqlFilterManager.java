package jp.co.future.uroborosql.filter;

import java.util.List;

/**
 * SQLフィルターの管理クラスインタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlFilterManager extends SqlFilter {

	/**
	 * SqlFilterの追加
	 *
	 * @param filter 追加するSqlFilter
	 * @return SqlFilterManager
	 */
	SqlFilterManager addSqlFilter(SqlFilter filter);

	/**
	 * SqlFilterのリストを取得します。
	 *
	 * @return SqlFilterのリスト
	 */
	List<SqlFilter> getFilters();

	/**
	 * SqlFilterのリストを設定します。
	 *
	 * @param filters
	 *            SqlFilterのリスト
	 */
	void setFilters(List<SqlFilter> filters);
}
