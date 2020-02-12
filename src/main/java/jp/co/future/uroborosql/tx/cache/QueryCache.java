package jp.co.future.uroborosql.tx.cache;

import java.util.Map;
import java.util.Set;

public interface QueryCache<K, E> {

	Class<E> getEntityType();

	Class<K> getKeyType();

	boolean containsKey(K key);

	E get(K key);

	Map<K, E> getAll();

	Map<K, E> getAll(Set<K> keys);

	void put(K key, E value);

	void putAll(Map<? extends K, ? extends E> map);

	boolean remove(K key);

	void removeAll(Set<K> keys);

	void clear();

	QueryCacheManager getCacheManager();

	boolean isClosed();

	void close();

}
