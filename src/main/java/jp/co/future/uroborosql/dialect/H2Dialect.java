/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;


/**
 * H2用のDialect
 *
 * @author H.Sugimoto
 */
public class H2Dialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public H2Dialect() {
		super();
	}

	@Override
	public String getDatabaseName() {
		return "H2";
	}
}
