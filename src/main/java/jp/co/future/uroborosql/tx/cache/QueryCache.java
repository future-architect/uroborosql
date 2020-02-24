/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
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

	void clear();

	void close();

}
