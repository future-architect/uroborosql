package jp.co.future.uroborosql.mapping;

import jp.co.future.uroborosql.config.SqlConfig;

/**
 * タイムスタンプによる楽観ロックサプライヤクラス
 *
 * @author H.Sugimoto
 */
public class TimestampOptimisticLockSupplier extends OptimisticLockSupplier {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.OptimisticLockSupplier#getPart(jp.co.future.uroborosql.mapping.TableMetadata.Column, jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public String getPart(final TableMetadata.Column versionColumn, final SqlConfig sqlConfig) {
		return versionColumn.getColumnIdentifier() + " = " + System.currentTimeMillis();
	}

}
