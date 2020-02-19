package jp.co.future.uroborosql.tx.cache;

import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.co.future.uroborosql.mapping.TableMetadata;

public interface QueryCache<E> {

	Class<E> getEntityType();

	TableMetadata getMetadata();

	boolean containsKey(List<Object> key);

	List<Object> getKey(E entity);

	E getEntity(List<Object> key);

	Map<List<Object>, E> getAll();

	Map<List<Object>, E> getAll(Set<List<Object>> keys);

	void put(E entity);

	void putAll(List<E> entities);

	boolean remove(List<Object> key);

	boolean remove(E entity);

	void removeAll(Set<List<Object>> keys);

	void clear();

	void close();

}
