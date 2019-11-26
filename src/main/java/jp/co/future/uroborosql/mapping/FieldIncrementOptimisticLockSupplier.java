/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import jp.co.future.uroborosql.config.SqlConfig;

/**
 * フィールド値のインクリメントによる楽観ロックサプライヤクラス
 *
 * @author H.Sugimoto
 */
public class FieldIncrementOptimisticLockSupplier extends OptimisticLockSupplier {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.OptimisticLockSupplier#getPart(jp.co.future.uroborosql.mapping.TableMetadata.Column, jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public String getPart(final TableMetadata.Column versionColumn, final SqlConfig sqlConfig) {
		return versionColumn.getColumnIdentifier() + " = /*(" + versionColumn.getCamelColumnName()
				+ " + @java.lang.Short@valueOf(1))*/";
	}

}
