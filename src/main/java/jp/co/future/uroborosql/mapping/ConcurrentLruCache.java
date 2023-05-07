/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Function;

/**
 * 指定されたキャッシュ制限によって制限される単純なLRU（Least Recently Used）キャッシュ<br>
 * この実装は、キャッシュされた値を格納するための ConcurrentHashMap と、
 * キャッシュがフル容量のときにキーを並べ替えて最も使用頻度の低いキーを選択するための ConcurrentLinkedDeque によって実装されています<br>
 *
 * <p>
 * 本クラスはSpring frameworkのorg.springframework.util.ConcurrentLruCacheを参考にしています。
 * </p>
 *
 * @author H.Sugimoto
 * @param <K> キャッシュキーの型
 * @param <V> キャッシュの値の型
 */
public class ConcurrentLruCache<K, V> {

	private final int sizeLimit;

	private final ConcurrentHashMap<K, V> cache = new ConcurrentHashMap<>();

	private final ConcurrentLinkedDeque<K> queue = new ConcurrentLinkedDeque<>();

	private final ReadWriteLock lock = new ReentrantReadWriteLock();

	private volatile int size;

	/**
	 * コンストラクタ
	 *
	 * @param sizeLimit キャッシュに格納できる値の上限. (0はキャッシュを行わず、常に新しい値を生成することを示す)
	 */
	public ConcurrentLruCache(final int sizeLimit) {
		if (sizeLimit < 0) {
			throw new IllegalArgumentException("Cache size limit must not be negative.");
		}
		this.sizeLimit = sizeLimit;
	}

	/**
	 * キャッシュからエントリーを取得します. キーに該当する値が無い場合は生成関数を使って値を生成します.
	 *
	 * @param key エントリを取得するためのキー
	 * @param generator 与えられたキーに対する新しい値を生成する関数
	 * @return キャッシュされた値、あるいは新たに生成された値
	 */
	public V get(final K key, final Function<K, V> generator) {
		if (generator == null) {
			throw new IllegalArgumentException("Generator function must not be null.");
		}
		if (this.sizeLimit == 0) {
			// sizeLimitが0の場合はキャッシュせずに毎回生成関数を使って値を生成する
			return generator.apply(key);
		}

		var cached = this.cache.get(key);
		if (cached != null) {
			if (this.size < this.sizeLimit) {
				return cached;
			}
			this.lock.readLock().lock();
			try {
				if (this.queue.removeLastOccurrence(key)) {
					this.queue.offer(key);
				}
				return cached;
			} finally {
				this.lock.readLock().unlock();
			}
		}

		this.lock.writeLock().lock();
		try {
			// 同一キーでの同時読み取りが発生する場合があるので再試行を行い、値が取得出来たらその値を返却する。
			cached = this.cache.get(key);
			if (cached != null) {
				if (this.queue.removeLastOccurrence(key)) {
					this.queue.offer(key);
				}
				return cached;
			}
			// サイズの不一致を防ぐため、最初に値を生成する
			var value = generator.apply(key);
			if (this.size == this.sizeLimit) {
				var leastUsed = this.queue.poll();
				if (leastUsed != null) {
					this.cache.remove(leastUsed);
				}
			}
			this.queue.offer(key);
			this.cache.put(key, value);
			this.size = this.cache.size();
			return value;
		} finally {
			this.lock.writeLock().unlock();
		}
	}

	/**
	 * 与えられたキーがこのキャッシュに存在するかどうかを判断する.
	 *
	 * @param key 確認するキー
	 * @return キーが存在する場合 {@code true}, 一致するキーがなかった場合は {@code false} を返す.
	 */
	public boolean contains(final K key) {
		return this.cache.containsKey(key);
	}

	/**
	 * 与えられたキーと関連する値を即座に削除する.
	 *
	 * @param key エントリーを消去するためのキー
	 * @return キーが以前から存在していた場合 {@code true},一致するキーがなかった場合 {@code false} を返す.
	 */
	public boolean remove(final K key) {
		this.lock.writeLock().lock();
		try {
			var wasPresent = this.cache.remove(key) != null;
			this.queue.remove(key);
			this.size = this.cache.size();
			return wasPresent;
		} finally {
			this.lock.writeLock().unlock();
		}
	}

	/**
	 * このキャッシュからすべてのエントリーを即座に削除する.
	 */
	public void clear() {
		this.lock.writeLock().lock();
		try {
			this.cache.clear();
			this.queue.clear();
			this.size = 0;
		} finally {
			this.lock.writeLock().unlock();
		}
	}

	/**
	 * キャッシュの現在のサイズを返す.
	 *
	 * @return キャッシュサイズ
	 * @see #sizeLimit()
	 */
	public int size() {
		return this.size;
	}

	/**
	 * キャッシュの最大エントリ数を返す.(0はキャッシュを行わず、常に新しい値を生成することを示す)
	 *
	 * @return キャッシュの最大エントリ数
	 * @see #size()
	 */
	public int sizeLimit() {
		return this.sizeLimit;
	}

}
