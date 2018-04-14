/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;


/**
 * Oracle用のDialect
 *
 * @author H.Sugimoto
 */
public class OracleDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public OracleDialect() {
		super();
	}

	@Override
	public String getName() {
		return "Oracle";
	}

}
