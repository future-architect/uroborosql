package jp.co.future.uroborosql.tx.cache;

import java.util.Map;
import java.util.Set;

public class StandardQueryCache<K, E> implements QueryCache<K, E> {

	@Override
	public Class<E> getEntityType() {
		// TODO 自動生成されたメソッド・スタブ
		return null;
	}

	@Override
	public Class<K> getKeyType() {
		// TODO 自動生成されたメソッド・スタブ
		return null;
	}

	@Override
	public boolean containsKey(final K key) {
		// TODO 自動生成されたメソッド・スタブ
		return false;
	}

	@Override
	public E get(final K key) {
		// TODO 自動生成されたメソッド・スタブ
		return null;
	}

	@Override
	public Map<K, E> getAll() {
		// TODO 自動生成されたメソッド・スタブ
		return null;
	}

	@Override
	public Map<K, E> getAll(final Set<K> keys) {
		// TODO 自動生成されたメソッド・スタブ
		return null;
	}

	@Override
	public void put(final K key, final E value) {
		// TODO 自動生成されたメソッド・スタブ

	}

	@Override
	public void putAll(final Map<? extends K, ? extends E> map) {
		// TODO 自動生成されたメソッド・スタブ

	}

	@Override
	public boolean remove(final K key) {
		// TODO 自動生成されたメソッド・スタブ
		return false;
	}

	@Override
	public void removeAll(final Set<K> keys) {
		// TODO 自動生成されたメソッド・スタブ

	}

	@Override
	public void clear() {
		// TODO 自動生成されたメソッド・スタブ

	}

	@Override
	public QueryCacheManager getCacheManager() {
		// TODO 自動生成されたメソッド・スタブ
		return null;
	}

	@Override
	public boolean isClosed() {
		// TODO 自動生成されたメソッド・スタブ
		return false;
	}

	@Override
	public void close() {
		// TODO 自動生成されたメソッド・スタブ

	}

}
