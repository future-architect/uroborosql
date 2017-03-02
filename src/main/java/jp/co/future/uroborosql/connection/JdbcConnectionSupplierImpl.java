package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

/**
 * JDBCドライバーを使用したコネクション供給クラス<br>
 * 指定されたプロパティをもとにコネクションを都度生成する
 *
 * @author H.Sugimoto
 */
public class JdbcConnectionSupplierImpl implements ConnectionSupplier {

	private static final Map<String, String> props = new HashMap<>();

	private static final String PROPS_JDBC_URL = "jdbc.url";
	private static final String PROPS_JDBC_USER = "jdbc.user";
	private static final String PROPS_JDBC_PASSWORD = "jdbc.password";
	private static final String PROPS_JDBC_SCHEMA = "jdbc.schema";
	private static final String PROPS_AUTO_COMMIT = "autocommit";
	private static final String PROPS_READ_ONLY = "readonly";
	private static final String PROPS_TRANSACTION_ISOLATION = "transactionisolation";

	/**
	 * JDBCConnectionSupplierImplのコンストラクタ
	 *
	 * @param driver JDBCドライバークラス
	 * @param url JDBCURL
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 */
	public JdbcConnectionSupplierImpl(final String url, final String user, final String password) {
		this(url, user, password, null, false, false);
	}

	/**
	 * JDBCConnectionSupplierImplのコンストラクタ
	 *
	 * @param driver JDBCドライバークラス
	 * @param url JDBCURL
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 * @param schema JDBCスキーマ名
	 */
	public JdbcConnectionSupplierImpl(final String url, final String user, final String password,
			final String schema) {
		this(url, user, password, schema, false, false);
	}

	/**
	 * JDBCConnectionSupplierImplのコンストラクタ
	 *
	 * @param driver JDBCドライバークラス
	 * @param url JDBCURL
	 * @param user 接続ユーザ名
	 * @param password 接続パスワード
	 * @param schema JDBCスキーマ名
	 * @param autoCommit 自動コミットするかどうか
	 * @param readOnly 参照のみかどうか
	 */
	public JdbcConnectionSupplierImpl(final String url, final String user, final String password,
			final String schema,
			final boolean autoCommit, final boolean readOnly) {
		props.put(PROPS_JDBC_URL, url);
		props.put(PROPS_JDBC_USER, user);
		props.put(PROPS_JDBC_PASSWORD, password);
		props.put(PROPS_JDBC_SCHEMA, schema);

		props.put(PROPS_AUTO_COMMIT, Boolean.toString(autoCommit));
		props.put(PROPS_READ_ONLY, Boolean.toString(readOnly));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection()
	 */
	@Override
	public Connection getConnection() {
		try {
			String jdbcUrl = props.get(PROPS_JDBC_URL);
			String jdbcUser = props.get(PROPS_JDBC_USER);
			String jdbcPassword = props.get(PROPS_JDBC_PASSWORD);

			Connection connection = DriverManager.getConnection(jdbcUrl, jdbcUser, jdbcPassword);

			String schema = getSchema();
			if (schema != null) {
				connection.setSchema(schema);
			}
			connection.setAutoCommit(getAutoCommit());
			connection.setReadOnly(getReadOnly());
			int transactionIsolation = getTransactionIsolation();
			if (transactionIsolation > 0) {
				connection.setTransactionIsolation(transactionIsolation);
			}
			return connection;
		} catch (SQLException ex) {
			ex.printStackTrace();
			return null;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection(java.lang.String)
	 */
	@Override
	public Connection getConnection(final String alias) {
		throw new UnsupportedOperationException("複数のDBコネクション提供には対応していません");
	}

	/**
	 * JDBCスキーマ名を取得
	 *
	 * @return JDBCスキーマ名
	 */
	protected String getSchema() {
		return props.get(PROPS_JDBC_SCHEMA);
	}

	/**
	 * JDBCスキーマ名を設定
	 *
	 * @param schema スキーマ名
	 */
	public void setSchema(final String schema) {
		props.put(PROPS_JDBC_SCHEMA, schema);
	}

	/**
	 * AutoCommitオプションの指定
	 *
	 * @param autoCommit
	 *            AutoCommitを行う場合は<code>true</code>
	 */
	public void setDefaultAutoCommit(final boolean autoCommit) {
		props.put(PROPS_AUTO_COMMIT, Boolean.toString(autoCommit));
	}

	/**
	 * AutoCommitオプションの取得
	 *
	 * @return AutoCommitを行う場合は<code>true</code>. 初期値は<code>false</code>
	 */
	protected boolean getAutoCommit() {
		return Boolean.parseBoolean(props.get(PROPS_AUTO_COMMIT));
	}

	/**
	 * ReadOnlyオプションの指定
	 *
	 * @param readOnly
	 *            readOnlyを指定する場合は<code>true</code>
	 */
	public void setDefaultReadOnly(final boolean readOnly) {
		props.put(PROPS_READ_ONLY, Boolean.toString(readOnly));
	}

	/**
	 * ReadOnlyオプションの指定
	 *
	 * @return readOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	protected boolean getReadOnly() {
		return Boolean.parseBoolean(props.get(PROPS_READ_ONLY));
	}

	/**
	 * transactionIsolationオプションの指定
	 *
	 * @see Connection#TRANSACTION_READ_UNCOMMITTED
	 * @see Connection#TRANSACTION_READ_COMMITTED
	 * @see Connection#TRANSACTION_REPEATABLE_READ
	 * @see Connection#TRANSACTION_SERIALIZABLE
	 *
	 * @param readOnly
	 *            readOnlyを指定する場合は<code>true</code>
	 */
	public void setDefaultTransactionIsolation(final int transactionIsolation) {
		if (Connection.TRANSACTION_READ_UNCOMMITTED == transactionIsolation
				|| Connection.TRANSACTION_READ_COMMITTED == transactionIsolation
				|| Connection.TRANSACTION_REPEATABLE_READ == transactionIsolation
				|| Connection.TRANSACTION_SERIALIZABLE == transactionIsolation) {
			props.put(PROPS_TRANSACTION_ISOLATION, String.valueOf(transactionIsolation));
		} else {
			throw new IllegalArgumentException("Unsupported level [" + transactionIsolation + "]");
		}
	}

	/**
	 * transactionIsolationオプションの指定
	 *
	 * @return readOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	protected int getTransactionIsolation() {
		String val = props.get(PROPS_TRANSACTION_ISOLATION);
		return val == null ? -1 : Integer.parseInt(val);
	}
}
