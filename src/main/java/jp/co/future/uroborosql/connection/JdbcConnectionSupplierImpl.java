/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Objects;

import jp.co.future.uroborosql.exception.UroborosqlSQLException;

/**
 * JDBCドライバーを使用したコネクション供給クラス<br>
 * 指定されたプロパティをもとにコネクションを都度生成する
 *
 * @author H.Sugimoto
 */
public class JdbcConnectionSupplierImpl implements ConnectionSupplier {

	/** デフォルトDB接続情報 */
	private final JdbcConnectionContext defaultConnectionContext;

	/**
	 * コンストラクタ
	 *
	 * @param url JDBC URL(必須)
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 */
	@Deprecated
	public JdbcConnectionSupplierImpl(final String url, final String user, final String password) {
		this(ConnectionContextBuilder.jdbc(url, user, password));
	}

	/**
	 * コンストラクタ
	 *
	 * @param url JDBC URL(必須)
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 * @param schema JDBCスキーマ名
	 */
	@Deprecated
	public JdbcConnectionSupplierImpl(final String url, final String user, final String password, final String schema) {
		this(ConnectionContextBuilder.jdbc(url, user, password, schema));
	}

	/**
	 * コンストラクタ
	 *
	 * @param url JDBC URL(必須)
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 * @param schema JDBCスキーマ名
	 * @param autoCommit 自動コミットするかどうか
	 * @param readOnly 参照のみかどうか
	 */
	@Deprecated
	public JdbcConnectionSupplierImpl(final String url, final String user, final String password, final String schema,
			final boolean autoCommit, final boolean readOnly) {
		this(ConnectionContextBuilder.jdbc(url, user, password, schema).autoCommit(autoCommit)
				.readOnly(readOnly));
	}

	/**
	 * コンストラクタ
	 *
	 * @param connectionContext DB接続情報
	 */
	public JdbcConnectionSupplierImpl(final JdbcConnectionContext connectionContext) {
		this.defaultConnectionContext = connectionContext;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection()
	 */
	@Override
	public Connection getConnection() {
		return getConnection(defaultConnectionContext);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection(jp.co.future.uroborosql.connection.ConnectionContext)
	 */
	@Override
	public Connection getConnection(final ConnectionContext ctx) {
		if (!(ctx instanceof JdbcConnectionContext)) {
			throw new IllegalArgumentException("ctx must be of type JdbcConnectionContext.");
		}
		JdbcConnectionContext jdbcCtx = (JdbcConnectionContext) ctx;
		try {
			Connection connection = new MetadataCachedConnectionWrapper(
					DriverManager.getConnection(jdbcCtx.url(), jdbcCtx.toProperties()), ctx.cacheSchema());

			String schema = jdbcCtx.schema();
			if (schema != null && !Objects.equals(connection.getSchema(), schema)) {
				connection.setSchema(schema);
			}
			if (jdbcCtx.autoCommit() != connection.getAutoCommit()) {
				connection.setAutoCommit(jdbcCtx.autoCommit());
			}
			if (jdbcCtx.readOnly() != connection.isReadOnly()) {
				connection.setReadOnly(jdbcCtx.readOnly());
			}
			int transactionIsolation = jdbcCtx.transactionIsolation();
			if (transactionIsolation > 0 && transactionIsolation != connection.getTransactionIsolation()) {
				connection.setTransactionIsolation(transactionIsolation);
			}
			return connection;
		} catch (SQLException ex) {
			throw new UroborosqlSQLException("Connection[" + jdbcCtx.url() + "] can not be acquired.", ex);
		}
	}

	/**
	 * JDBCスキーマ名を設定
	 *
	 * @param schema スキーマ名
	 */
	@Deprecated
	public void setSchema(final String schema) {
		setDefaultSchema(schema);
	}

	/**
	 * デフォルトのDB接続情報にJDBCスキーマ名を設定
	 *
	 * @param schema スキーマ名
	 */
	public void setDefaultSchema(final String schema) {
		defaultConnectionContext.schema(schema);
	}

	/**
	 * デフォルトのDB接続情報にAutoCommitオプションの指定
	 *
	 * @param autoCommit AutoCommitを行う場合は<code>true</code>
	 */
	public void setDefaultAutoCommit(final boolean autoCommit) {
		defaultConnectionContext.autoCommit(autoCommit);
	}

	/**
	 * デフォルトのDB接続情報にReadOnlyオプションを指定
	 *
	 * @param readOnly readOnlyを指定する場合は<code>true</code>
	 */
	public void setDefaultReadOnly(final boolean readOnly) {
		defaultConnectionContext.readOnly(readOnly);
	}

	/**
	 * デフォルトのDB接続情報にtransactionIsolationオプションを指定
	 *
	 * @see Connection#TRANSACTION_READ_UNCOMMITTED
	 * @see Connection#TRANSACTION_READ_COMMITTED
	 * @see Connection#TRANSACTION_REPEATABLE_READ
	 * @see Connection#TRANSACTION_SERIALIZABLE
	 *
	 * @param transactionIsolation transactionIsolationオプション
	 */
	public void setDefaultTransactionIsolation(final int transactionIsolation) {
		defaultConnectionContext.transactionIsolation(transactionIsolation);
	}

	/**
	 * デフォルトのDB接続情報にスキーマ名のキャッシュオプションを指定
	 *
	 * @param cache スキーマ名をキャッシュする場合は<code>true</code>
	 */
	public void setDefaultCacheSchema(final boolean cache) {
		defaultConnectionContext.cacheSchema(cache);
	}
}
