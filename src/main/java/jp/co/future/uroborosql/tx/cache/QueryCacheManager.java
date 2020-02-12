package jp.co.future.uroborosql.tx.cache;

import java.sql.Connection;

public interface QueryCacheManager {
	<K, E> QueryCache<K, E> createCache(Connection conn, Class<E> entityType, Class<K> keyType);

	<K, E> QueryCache<K, E> createCache(Connection conn, QueryCache<K, E> originalCache);

	<E> void destroyCache(Connection conn, Class<E> entityType);

	<K, E> QueryCache<K, E> getCache(Connection conn, Class<E> entityType, Class<K> keyType);

	boolean isClosed();

	void close();
}
