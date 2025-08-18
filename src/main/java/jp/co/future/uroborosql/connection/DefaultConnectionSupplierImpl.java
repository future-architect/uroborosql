/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * デフォルトコネクション供給クラス<br>
 * コネクションを保持して返すだけの実装。<br>
 * 保持しているコネクションはクローズされないようWrapper経由で提供される。<br>
 *
 * @author H.Sugimoto
 */
public class DefaultConnectionSupplierImpl implements ConnectionSupplier {
	/**
	 * コネクションのスキーマ名の取り扱いを指定するオプション
	 */
	public enum SchemaOption {
		/** キャッシュしない */
		NONE,
		/** キャッシュする */
		CACHE,
		/** 固定する */
		FIX
	}

	/**  コネクション */
	private final Connection connection;

	/** スキーマ名を固定する場合のスキーマ名 */
	private String fixedSchemaName = null;

	/**
	 * コンストラクタ.
	 *
	 * @param connection コネクション
	 */
	public DefaultConnectionSupplierImpl(final Connection connection) {
		this(connection, SchemaOption.NONE);
	}

	/**
	 * コンストラクタ.
	 *
	 * @param connection コネクション
	 * @param schemaOption コネクションのスキーマ名の取り扱いを指定するオプション
	 */
	public DefaultConnectionSupplierImpl(final Connection connection, final SchemaOption schemaOption) {
		if (schemaOption == SchemaOption.FIX) {
			if (fixedSchemaName == null) {
				try {
					fixedSchemaName = connection.getSchema();
				} catch (SQLException ex) {
					throw new UnsupportedOperationException(ex);
				}
			}
			this.connection = new SchemaFixedConnectionWrapper(new CloseIgnoringConnectionWrapper(connection),
					fixedSchemaName);
		} else {
			this.connection = new MetadataCachedConnectionWrapper(new CloseIgnoringConnectionWrapper(connection),
					schemaOption == SchemaOption.CACHE);
		}
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
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#setDefaultCacheSchema(boolean)
	 */
	@Override
	public void setDefaultCacheSchema(final boolean cache) {
		try {
			if (connection.isWrapperFor(MetadataCachedConnectionWrapper.class)) {
				connection.unwrap(MetadataCachedConnectionWrapper.class).setCacheSchema(cache);
			} else {
				throw new UnsupportedOperationException("Schema is fixed.");
			}
		} catch (SQLException ex) {
			throw new IllegalStateException(ex);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#setDefaultFixSchema(boolean)
	 */
	@Override
	public void setDefaultFixSchema(final boolean fixed) {
		throw new UnsupportedOperationException();
	}

}
