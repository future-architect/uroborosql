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
	/** ENTITY_INSERT */
	ENTITY_INSERT,
	/** UPDATE */
	UPDATE,
	/** ENTITY_UPDATE */
	ENTITY_UPDATE,
	/** DELETE */
	DELETE,
	/** ENTITY_DELETE */
	ENTITY_DELETE,
	/** SELECT */
	SELECT,
	/** ENTITY_SELECT */
	ENTITY_SELECT,
	/** BATCH_INSERT */
	BATCH_INSERT,
	/** BULK_INSERT */
	BULK_INSERT,
	/** BATCH_UPDATE */
	BATCH_UPDATE,
	/** PROCEDURE */
	PROCEDURE,
	/** TRUNCATE */
	TRUNCATE,
	/** NONE */
	NONE,
}