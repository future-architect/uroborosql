/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.enums;

/**
 * 主キー生成戦略の型
 *
 * @author H.Sugimoto
 * @since v0.12.0
 */
public enum GenerationType {
	/** データベースのIDカラムを使用してエンティティの主キーに値を割り当てる */
	IDENTITY,
	/** データベースのシーケンスを使用してエンティティの主キーに値を割り当てる */
	SEQUENCE
}
