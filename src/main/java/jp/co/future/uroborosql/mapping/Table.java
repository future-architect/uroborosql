/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import org.apache.commons.lang3.StringUtils;

/**
 * テーブル情報
 *
 * @author ota
 */
public interface Table {
	/**
	 * テーブル名取得
	 *
	 * @return テーブル名
	 */
	String getName();

	/**
	 * スキーマ名取得
	 *
	 * @return スキーマ名
	 */
	String getSchema();

	/**
	 * テーブル識別名の取得
	 *
	 * @return テーブル識別名
	 */
	default String getIdentifier() {
		if (StringUtils.isEmpty(getSchema())) {
			return getName();
		} else {
			return getSchema() + "." + getName();
		}
	}
}