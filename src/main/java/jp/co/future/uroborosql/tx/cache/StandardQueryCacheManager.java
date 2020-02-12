package jp.co.future.uroborosql.tx.cache;

import java.sql.Connection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class StandardQueryCacheManager implements QueryCacheManager {
	private boolean closed = false;

	private final Map<Connection, Map<Class<?>, QueryCache<?, ?>>> connMap = new ConcurrentHashMap<>();

	private static final QueryCacheManager INSTANCE = new StandardQueryCacheManager();

	public static QueryCacheManager getInstance() {
		return INSTANCE;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <K, E> QueryCache<K, E> createCache(final Connection conn, final Class<E> entityType,
			final Class<K> keyType) {
		Map<Class<?>, QueryCache<?, ?>> cacheMap = connMap.getOrDefault(conn,
				new ConcurrentHashMap<Class<?>, QueryCache<?, ?>>());
		QueryCache<?, ?> cache = cacheMap.computeIfAbsent(entityType, k -> new StandardQueryCache<K, E>());
		return (QueryCache<K, E>) cache;
	}

	@Override
	public <K, E> QueryCache<K, E> createCache(final Connection conn, final QueryCache<K, E> originalCache) {
		// TODO 自動生成されたメソッド・スタブ
		return null;
	}

	@Override
	public <E> void destroyCache(final Connection conn, final Class<E> entityType) {
		// TODO 自動生成されたメソッド・スタブ

	}

	@Override
	public <K, E> QueryCache<K, E> getCache(final Connection conn, final Class<E> entityType, final Class<K> keyType) {
		// TODO 自動生成されたメソッド・スタブ
		return null;
	}

	@Override
	public boolean isClosed() {
		return this.closed;
	}

	@Override
	public void close() {
		this.closed = true;
	}

}
