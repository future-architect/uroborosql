/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.util.stream.Stream;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * EntityQuery後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterEntityQueryEvent extends EntityExecutionEvent {
	/** 検索結果Stream. */
	private Stream<?> results;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param entity Entity
	 * @param entityType EntityType
	 * @param results Query Result Stream
	 */
	public AfterEntityQueryEvent(final ExecutionContext executionContext, final Object entity,
			final Class<?> entityType,
			final Stream<?> results) {
		super(executionContext, entity, entityType);
		this.results = results;
	}

	/**
	 * 検索結果Streamの取得.
	 * @return 検索結果Stream
	 */
	public Stream<?> getResults() {
		return results;
	}

	/**
	 * 検索結果Streamの設定.
	 * @param results 検索結果Stream
	 */
	public void setResults(final Stream<?> results) {
		this.results = results;
	}

}
