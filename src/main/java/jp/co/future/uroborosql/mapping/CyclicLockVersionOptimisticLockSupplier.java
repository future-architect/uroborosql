package jp.co.future.uroborosql.mapping;

import jp.co.future.uroborosql.config.SqlConfig;

/**
 * 循環式ロックバージョンによる楽観ロックサプライヤクラス
 *
 * @author H.Sugimoto
 */
public class CyclicLockVersionOptimisticLockSupplier extends OptimisticLockSupplier {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.OptimisticLockSupplier#getPart(jp.co.future.uroborosql.mapping.TableMetadata.Column, jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public String getPart(final TableMetadata.Column versionColumn, final SqlConfig sqlConfig) {
		String modPart = sqlConfig.getDialect().getModLiteral(versionColumn.getColumnIdentifier(),
				"1" + new String(new char[versionColumn.getColumnSize() - 1]).replace("\0", "0"));
		return versionColumn.getColumnIdentifier() + " = (" + modPart + ") + 1";
	}

}
