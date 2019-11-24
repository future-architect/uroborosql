package jp.co.future.uroborosql.mapping;

import jp.co.future.uroborosql.config.SqlConfig;

/**
 * ロックバージョンによる楽観ロックサプライヤクラス
 *
 * @author H.Sugimoto
 */
public class LockVersionOptimisticLockSupplier extends OptimisticLockSupplier {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.OptimisticLockSupplier#getPart(jp.co.future.uroborosql.mapping.TableMetadata.Column, jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public String getPart(final TableMetadata.Column versionColumn, final SqlConfig sqlConfig) {
		return versionColumn.getColumnIdentifier() + " = " + versionColumn.getColumnIdentifier() + " + 1";
	}

}
