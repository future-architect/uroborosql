/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.time.LocalDateTime;
import java.util.EventObject;

/**
 * Uroborosqlで発行されるイベントオブジェクトの抽象親クラス
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public abstract class AbstractEvent extends EventObject {
	/** イベント開始日時. */
	private final LocalDateTime occurredOn = LocalDateTime.now();

	/**
	 * コンストラクタ.
	 *
	 * @param source 対象ソース
	 */
	protected AbstractEvent(final Object source) {
		super(source);
	}

	/** イベント開始日時の取得. */
	public LocalDateTime occurredOn() {
		return this.occurredOn;
	}
}
