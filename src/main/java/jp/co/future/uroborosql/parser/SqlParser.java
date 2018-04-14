/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parser;

/**
 * SQL解析インターフェース
 *
 * @author H.Sugimoto
 */
public interface SqlParser {
	/**
	 * SQL解析<br>
	 * SQL文の内容を解析し、変換クラスを生成する
	 *
	 * @return コンテキスト変換器
	 */
	ContextTransformer parse();
}