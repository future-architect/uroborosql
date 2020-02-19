package jp.co.future.uroborosql.tx.cache;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.utils.BeanAccessor;

public class StandardQueryCache<E> implements QueryCache<E> {
	private final Map<List<Object>, E> cache;

	private final Class<E> entityType;
	private TableMetadata metadata;
	private List<? extends TableMetadata.Column> keyColumns;

	public StandardQueryCache(final Class<E> entityType, final TableMetadata metadata) {
		this.entityType = entityType;
		this.metadata = metadata;
		this.cache = new ConcurrentHashMap<>(100);
		this.keyColumns = metadata.getKeyColumns();
	}

	public StandardQueryCache(final Class<E> entityType, final QueryCache<? extends E> original) {
		this(entityType, original.getMetadata());
		cache.putAll(original.getAll());

	}

	@Override
	public Class<E> getEntityType() {
		return entityType;
	}

	@Override
	public TableMetadata getMetadata() {
		return metadata;
	}

	@Override
	public boolean containsKey(final List<Object> key) {
		return cache.containsKey(key);
	}

	@Override
	public List<Object> getKey(final E entity) {
		Map<String, Object> entityMap = BeanAccessor.asMap(entity);
		return keyColumns.stream()
				.map(c -> entityMap.get(c.getCamelColumnName()))
				.collect(Collectors.toList());
	}

	@Override
	public E getEntity(final List<Object> key) {
		return cache.get(key);
	}

	@Override
	public Map<List<Object>, E> getAll() {
		return cache.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
	}

	@Override
	public Map<List<Object>, E> getAll(final Set<List<Object>> keys) {
		if (keys == null || keys.isEmpty()) {
			return Collections.emptyMap();
		} else {
			return cache.entrySet().stream().filter(e -> keys.contains(e.getKey()))
					.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
		}
	}

	@Override
	public void put(final E entity) {
		if (entity == null) {
			throw new IllegalArgumentException("entity does not allow null.");
		}
		List<Object> key = getKey(entity);
		if (!key.isEmpty()) {
			this.cache.put(key, entity);
		}
	}

	@Override
	public void putAll(final List<E> entities) {
		if (entities == null) {
			throw new IllegalArgumentException("entities does not allow null.");
		}
		entities.stream().forEach(this::put);

	}

	@Override
	public boolean remove(final List<Object> key) {
		if (key != null) {
			E value = this.cache.remove(key);
			return value != null;
		} else {
			return false;
		}
	}

	@Override
	public boolean remove(final E entity) {
		List<Object> key = getKey(entity);
		if (!key.isEmpty()) {
			return this.remove(key);
		} else {
			return false;
		}
	}

	@Override
	public void removeAll(final Set<List<Object>> keys) {
		if (keys != null) {
			keys.forEach(k -> this.cache.remove(k));
		}
	}

	@Override
	public void clear() {
		this.cache.clear();
	}

	@Override
	public void close() {
		this.clear();
	}

}
