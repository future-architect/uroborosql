/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.Connection;

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
	 * コンストラクタ.
	 *
	 * @param connection コネクション
	 */
	public DefaultConnectionSupplierImpl(final Connection connection) {
		this(connection, false);
	}

	/**
	 * コンストラクタ.
	 *
	 * @param connection コネクション
	 * @param cacheSchema スキーマ名をキャッシュするかどうか. キャッシュする場合は<code>true</code>.
	 */
	public DefaultConnectionSupplierImpl(final Connection connection, final boolean cacheSchema) {
		this.connection = new MetadataCachedConnectionWrapper(new CloseIgnoringConnectionWrapper(connection),
				cacheSchema);
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
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection(jp.co.future.uroborosql.connection.ConnectionContext)
	 */
	@Override
	public Connection getConnection(final ConnectionContext ctx) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 保持しているConnectionにスキーマ名のキャッシュオプションを指定
	 *
	 * @param cache スキーマ名をキャッシュする場合は<code>true</code>
	 */
	public void setCacheSchema(final boolean cache) {
		((MetadataCachedConnectionWrapper) connection).setCacheSchema(cache);
	}

}
