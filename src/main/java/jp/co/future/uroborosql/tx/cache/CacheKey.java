package jp.co.future.uroborosql.tx.cache;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * QueryCacheのキーを表すクラス
 *
 * @author H.Sugimoto
 */
public final class CacheKey {
	/** キー値のList */
	private final List<Object> key;

	/**
	 * コンストラクタ
	 * @param key キーの集合
	 */
	public CacheKey(final List<Object> key) {
		if (key == null || key.isEmpty()) {
			throw new IllegalArgumentException("key is required.");
		}
		this.key = key;
	}

	/**
	 * コンストラクタ
	 * @param key キー配列
	 */
	public CacheKey(final Object... key) {
		if (key == null || key.length == 0) {
			throw new IllegalArgumentException("key is required.");
		}
		this.key = Arrays.asList(key);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof CacheKey)) {
			return false;
		}

		List<Object> thisKey = key;
		List<Object> otherKey = ((CacheKey) obj).key;

		if (thisKey.size() != otherKey.size()) {
			return false;
		}

		for (int i = 0; i < thisKey.size(); i++) {
			if (!Objects.toString(thisKey.get(i)).equals(Objects.toString(otherKey.get(i)))) {
				return false;
			}
		}

		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		for (Object obj : key) {
			result = prime * result + (obj == null ? 0 : obj.hashCode());
		}
		return result;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return key.toString();
	}

}
