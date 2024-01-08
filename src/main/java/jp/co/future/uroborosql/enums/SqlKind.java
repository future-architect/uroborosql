/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.enums;

/**
 * SQL種別
 */
public enum SqlKind {
	/** INSERT */
	INSERT,
	/** UPDATE */
	UPDATE,
	/** MERGE */
	MERGE,
	/** DELETE */
	DELETE,
	/** SELECT */
	SELECT,
	/** ENTITY_INSERT */
	ENTITY_INSERT,
	/** ENTITY_UPDATE */
	ENTITY_UPDATE,
	/** ENTITY_DELETE */
	ENTITY_DELETE,
	/** ENTITY_SELECT */
	ENTITY_SELECT,
	/** BATCH_INSERT */
	BATCH_INSERT,
	/** BULK_INSERT */
	BULK_INSERT,
	/** ENTITY_BATCH_INSERT */
	ENTITY_BATCH_INSERT,
	/** ENTITY_BULK_INSERT */
	ENTITY_BULK_INSERT,
	/** BATCH_UPDATE */
	BATCH_UPDATE,
	/** PROCEDURE */
	PROCEDURE,
	/** TRUNCATE */
	TRUNCATE,
	/** NONE */
	NONE;

	/**
	 * Entity型に対するSQL種別かどうかを返す.
	 *
	 * @return Entity型に対するSQL種別の場合<code>true</code>
	 */
	public boolean isEntityType() {
		return name().startsWith("ENTITY");
	}
}