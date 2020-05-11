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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jp.co.future.uroborosql.exception.UroborosqlSQLException;

/**
 * JDBCドライバーを使用したコネクション供給クラス<br>
 * 指定されたプロパティをもとにコネクションを都度生成する
 *
 * @author H.Sugimoto
 */
public class JdbcConnectionSupplierImpl implements ConnectionSupplier {

	public static final String PROPS_JDBC_URL = "jdbc.url";
	public static final String PROPS_JDBC_USER = "jdbc.user";
	public static final String PROPS_JDBC_PASSWORD = "jdbc.password";
	public static final String PROPS_JDBC_SCHEMA = "jdbc.schema";

	private final Map<String, String> defaultConnProps = new ConcurrentHashMap<>();

	/**
	 * JDBCConnectionSupplierImplのコンストラクタ
	 *
	 * @param url JDBC URL(必須)
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 */
	public JdbcConnectionSupplierImpl(final String url, final String user, final String password) {
		this(url, user, password, null, false, false);
	}

	/**
	 * JDBCConnectionSupplierImplのコンストラクタ
	 *
	 * @param url JDBC URL(必須)
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 * @param schema JDBCスキーマ名
	 */
	public JdbcConnectionSupplierImpl(final String url, final String user, final String password, final String schema) {
		this(url, user, password, schema, false, false);
	}

	/**
	 * JDBCConnectionSupplierImplのコンストラクタ
	 *
	 * @param url JDBC URL(必須)
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 * @param schema JDBCスキーマ名
	 * @param autoCommit 自動コミットするかどうか
	 * @param readOnly 参照のみかどうか
	 */
	public JdbcConnectionSupplierImpl(final String url, final String user, final String password, final String schema,
			final boolean autoCommit, final boolean readOnly) {
		if (url == null) {
			throw new IllegalArgumentException("url is required but null.");
		}
		defaultConnProps.put(PROPS_JDBC_URL, url);
		if (user != null) {
			defaultConnProps.put(PROPS_JDBC_USER, user);
		}
		if (password != null) {
			defaultConnProps.put(PROPS_JDBC_PASSWORD, password);
		}
		if (schema != null) {
			defaultConnProps.put(PROPS_JDBC_SCHEMA, schema);
		}
		setAutoCommit(defaultConnProps, autoCommit);
		setReadOnly(defaultConnProps, readOnly);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection()
	 */
	@Override
	public Connection getConnection() {
		return getConnection(defaultConnProps);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection(java.util.Map)
	 */
	@Override
	public Connection getConnection(final Map<String, String> props) {
		String jdbcUrl = props.get(PROPS_JDBC_URL);
		String jdbcUser = props.get(PROPS_JDBC_USER);
		String jdbcPassword = props.get(PROPS_JDBC_PASSWORD);
		try {
			Connection connection = DriverManager.getConnection(jdbcUrl, jdbcUser, jdbcPassword);

			String schema = props.get(PROPS_JDBC_SCHEMA);
			if (schema != null) {
				connection.setSchema(schema);
			}
			connection.setAutoCommit(isAutoCommit(props));
			connection.setReadOnly(isReadOnly(props));
			int transactionIsolation = getTransactionIsolation(props);
			if (transactionIsolation > 0) {
				connection.setTransactionIsolation(transactionIsolation);
			}
			return connection;
		} catch (SQLException ex) {
			throw new UroborosqlSQLException("Connection[" + jdbcUrl + "] can not be acquired.", ex);
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
		defaultConnProps.put(PROPS_JDBC_SCHEMA, schema);
	}

	/**
	 * デフォルトのDB接続情報にAutoCommitオプションの指定
	 *
	 * @param autoCommit
	 *            AutoCommitを行う場合は<code>true</code>
	 */
	public void setDefaultAutoCommit(final boolean autoCommit) {
		setAutoCommit(defaultConnProps, autoCommit);
	}

	/**
	 * デフォルトのDB接続情報にReadOnlyオプションを指定
	 *
	 * @param readOnly
	 *            readOnlyを指定する場合は<code>true</code>
	 */
	public void setDefaultReadOnly(final boolean readOnly) {
		setReadOnly(defaultConnProps, readOnly);
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
		setTransactionIsolation(defaultConnProps, transactionIsolation);
	}
}
