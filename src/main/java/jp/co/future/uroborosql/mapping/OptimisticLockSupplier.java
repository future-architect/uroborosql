package jp.co.future.uroborosql.mapping;

import java.util.Map;
import java.util.ServiceLoader;
import java.util.concurrent.ConcurrentHashMap;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * 楽観ロックサプライヤ
 *
 * @author H.Sugimoto
 */
public abstract class OptimisticLockSupplier {
	private static final Map<Class<? extends OptimisticLockSupplier>, OptimisticLockSupplier> suppliers = new ConcurrentHashMap<>();

	static {
		for (OptimisticLockSupplier supplier : ServiceLoader.load(OptimisticLockSupplier.class)) {
			suppliers.put(supplier.getClass(), supplier);
		}
	}

	/**
	 * 楽観ロックサプライヤの取得
	 *
	 * @return 楽観ロックサプライヤ
	 * @throws UroborosqlRuntimeException 指定した楽観ロックサプライヤがServiceLoaderに登録されていない場合
	 */
	public static OptimisticLockSupplier getSupplier(final Class<? extends OptimisticLockSupplier> supplier) {
		if (suppliers.containsKey(supplier)) {
			return suppliers.get(supplier);
		} else {
			throw new UroborosqlRuntimeException(
					"OptimisticLockSupplier not found in ServiceLoader. class=" + supplier.toString());
		}
	}

	/**
	 * バージョンカラムの設定を行うためのSQLパーツを取得する.
	 *
	 * @param versionColumn バージョンカラム
	 * @param sqlConfig SqlConfig
	 * @return バージョンカラムの設定を行うためのSQLパーツ
	 */
	public abstract String getPart(final TableMetadata.Column versionColumn, final SqlConfig sqlConfig);

}
