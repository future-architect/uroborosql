/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
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
