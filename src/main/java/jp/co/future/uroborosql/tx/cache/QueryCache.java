package jp.co.future.uroborosql.tx.cache;

import java.util.Map;

import jp.co.future.uroborosql.mapping.TableMetadata;

public interface QueryCache<E> {

	Class<E> getEntityType();

	TableMetadata getMetadata();

	boolean containsKey(CacheKey key);

	CacheKey getKey(E entity);

	E getEntity(CacheKey key);

	Map<CacheKey, E> getAll();

	void put(E entity);

	boolean remove(CacheKey key);

	boolean remove(E entity);

	void clear();

	void close();

}
