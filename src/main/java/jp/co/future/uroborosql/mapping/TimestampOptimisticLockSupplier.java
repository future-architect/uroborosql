/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import java.sql.Types;

import jp.co.future.uroborosql.config.SqlConfig;

/**
 * タイムスタンプによる楽観ロックサプライヤクラス
 *
 * @author H.Sugimoto
 */
public class TimestampOptimisticLockSupplier extends OptimisticLockSupplier {
	/**
	 * バージョンカラムの値生成時にZoneIdを渡す場合に設定するパラメータキー名 : {@value}
	 */
	public static final String PARAM_KEY_ZONE_ID = "_zoneId";

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.OptimisticLockSupplier#getPart(jp.co.future.uroborosql.mapping.TableMetadata.Column, jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public String getPart(final TableMetadata.Column versionColumn, final SqlConfig sqlConfig) {
		if (Types.TIMESTAMP == versionColumn.getDataType()) {
			return versionColumn.getColumnIdentifier() + " = /*SF.nowTimestamp(" + PARAM_KEY_ZONE_ID + ")*/";
		} else if (Types.TIMESTAMP_WITH_TIMEZONE == versionColumn.getDataType()) {
			return versionColumn.getColumnIdentifier() + " = /*SF.nowTimestampWithZone(" + PARAM_KEY_ZONE_ID + ")*/";
		} else {
			return versionColumn.getColumnIdentifier() + " = " + System.currentTimeMillis();
		}
	}

}
