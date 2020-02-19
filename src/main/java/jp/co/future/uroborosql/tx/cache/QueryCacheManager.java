package jp.co.future.uroborosql.tx.cache;

import java.util.Optional;

import jp.co.future.uroborosql.mapping.TableMetadata;

public interface QueryCacheManager {
	/**
	 * 検索結果キャッシュの取得
	 *
	 * @param E エンティティ
	 * @param entityType エンティティ型
	 * @param metadata TableMetadata
	 * @return 検索結果キャッシュ
	 */
	<E> Optional<QueryCache<E>> getQueryCache(final Class<E> entityType, TableMetadata metadata);

}
