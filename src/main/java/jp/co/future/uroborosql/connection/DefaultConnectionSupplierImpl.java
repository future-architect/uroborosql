/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.*;

/**
 * デフォルトコネクション供給クラス<br>
 * コネクションを保持して返すだけの実装。<br>
 * 保持しているコネクションはクローズされないようWrapper経由で提供される。<br>
 *
 * @author H.Sugimoto
 */
public class DefaultConnectionSupplierImpl implements ConnectionSupplier {
	/**  コネクション */
	private final Connection connection;

	/**
	 * コンストラクタ。
	 * @param connection コネクション
	 */
	public DefaultConnectionSupplierImpl(final Connection connection) {
		this.connection = new DoNotCloseConnectionWrapper(connection);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection()
	 */
	@Override
	public Connection getConnection() {
		return connection;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection(java.lang.String)
	 */
	@Override
	public Connection getConnection(final String alias) {
		return connection;
	}

}
