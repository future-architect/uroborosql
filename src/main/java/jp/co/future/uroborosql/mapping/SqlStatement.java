/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

/**
 * SQLのStatementを表すEnum
 *
 * @author H.Sugimoto
 * @since v0.6.1
 *
 */
public enum SqlStatement {
	SELECT, INSERT, UPDATE, DELETE, NONE;
}
