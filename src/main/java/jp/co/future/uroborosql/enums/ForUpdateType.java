/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.enums;

/**
 * SELECT FOR UPDATEのオプションを表す列挙型
 *
 * @author H.Sugimoto
 * @since v0.14.0
 */
public enum ForUpdateType {
	/** 標準 */
	NORMAL,
	/** 待機 */
	WAIT,
	/** 待機しない */
	NOWAIT;
}
