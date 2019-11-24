package jp.co.future.uroborosql.mapping;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.mapping.TableMetadata.Column;

/**
 * 例外検証のカスタム楽観ロックサプライヤ
 *
 * @author H.Sugimoto
 */
public class CustomOptimisticLockSupplier extends OptimisticLockSupplier {

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.OptimisticLockSupplier#getPart(jp.co.future.uroborosql.mapping.TableMetadata.Column, jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public String getPart(final Column versionColumn, final SqlConfig sqlConfig) {
		return null;
	}

}
