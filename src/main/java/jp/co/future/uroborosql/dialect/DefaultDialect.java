/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import jp.co.future.uroborosql.connection.ConnectionSupplier;

/**
 * 標準のDialect
 *
 * @author H.Sugimoto
 */
public class DefaultDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public DefaultDialect() {
		super();
	}

	@Override
	public String getName() {
		return "default";
	}

	@Override
	public boolean accept(ConnectionSupplier supplier) {
		return true;
	}
}
