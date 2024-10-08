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
		var jdbcCtx = (JdbcConnectionContext) ctx;
		try {
			var connection = DriverManager.getConnection(jdbcCtx.url(), jdbcCtx.toProperties());

			var schema = jdbcCtx.schema();
			if (schema != null && !Objects.equals(connection.getSchema(), schema)) {
				connection.setSchema(schema);
			}
			if (jdbcCtx.autoCommit() != connection.getAutoCommit()) {
				connection.setAutoCommit(jdbcCtx.autoCommit());
			}
			if (jdbcCtx.readOnly() != connection.isReadOnly()) {
				connection.setReadOnly(jdbcCtx.readOnly());
			}
			var transactionIsolation = jdbcCtx.transactionIsolation();
			if (transactionIsolation > 0 && transactionIsolation != connection.getTransactionIsolation()) {
				connection.setTransactionIsolation(transactionIsolation);
			}
			return connection;
		} catch (SQLException ex) {
			throw new UroborosqlSQLException("Connection[" + jdbcCtx.url() + "] can not be acquired.", ex);
		}
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
}
