package jp.co.future.uroborosql.tx.cache;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.utils.BeanAccessor;

public class StandardQueryCache<E> implements QueryCache<E> {
	private final Map<CacheKey, E> cache;

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
		original.getAll().values().stream().forEach(v -> this.put(v));
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
	public boolean containsKey(final CacheKey key) {
		if (key == null) {
			return false;
		}
		return cache.containsKey(key);
	}

	@Override
	public CacheKey getKey(final E entity) {
		Map<String, Object> entityMap = BeanAccessor.asMap(entity);
		return new CacheKey(keyColumns.stream()
				.map(c -> entityMap.get(c.getCamelColumnName()))
				.collect(Collectors.toList()));
	}

	@Override
	public E getEntity(final CacheKey key) {
		return cache.get(key);
	}

	@Override
	public Map<CacheKey, E> getAll() {
		return cache.entrySet().stream().collect(Collectors.toMap(e -> e.getKey(), Map.Entry::getValue));
	}

	@Override
	public void put(final E entity) {
		if (entity == null) {
			throw new IllegalArgumentException("entity does not allow null.");
		}
		this.cache.put(getKey(entity), entity);
	}

	@Override
	public boolean remove(final CacheKey key) {
		if (key != null) {
			E value = this.cache.remove(key);
			return value != null;
		} else {
			return false;
		}
	}

	@Override
	public boolean remove(final E entity) {
		return this.remove(getKey(entity));
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
