package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * データソースを使用したコネクション供給クラス<br>
 * 指定されたデータソースからコネクションを取得する
 *
 * @author H.Sugimoto
 */
public class DataSourceConnectionSupplierImpl implements ConnectionSupplier {
	private String dataSourceName;

	private final Map<String, DataSource> dsMap = new ConcurrentHashMap<>();

	private final Map<String, Map<String, String>> connProps = new ConcurrentHashMap<>();

	private static final String PROPS_AUTO_COMMIT = "AUTO_COMMIT";
	private static final String PROPS_READ_ONLY = "READ_ONLY";
	private static final String PROPS_TRANSACTION_ISOLATION = "TRANSACTION_ISOLATION";

	/**
	 * コンストラクタ。
	 */
	public DataSourceConnectionSupplierImpl() {
		this(null);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param dataSourceName 取得するコネクションのデータソース名
	 */
	public DataSourceConnectionSupplierImpl(final String dataSourceName) {
		this.dataSourceName = dataSourceName;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection()
	 */
	@Override
	public Connection getConnection() {
		return getConnection(getDataSourceName());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @param dsName 取得するコネクションのデータソース名
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection(java.lang.String)
	 */
	@Override
	public Connection getConnection(final String dsName) {
		try {
			DataSource ds = dsMap.computeIfAbsent(dsName, DataSourceConnectionSupplierImpl::getNewDataSource);
			Connection connection = ds.getConnection();
			boolean autoCommit = getAutoCommit(dsName);
			if (connection.getAutoCommit() != autoCommit) {
				connection.setAutoCommit(autoCommit);
			}
			boolean readOnly = getReadOnly(dsName);
			if (connection.isReadOnly() != readOnly) {
				connection.setReadOnly(readOnly);
			}
			int transactionIsolation = getTransactionIsolation(dsName);
			if (transactionIsolation > 0 && connection.getTransactionIsolation() != transactionIsolation) {
				connection.setTransactionIsolation(transactionIsolation);
			}
			return connection;
		} catch (SQLException ex) {
			throw new UroborosqlRuntimeException("Connection[" + dsName + "] can not be acquired.", ex);
		}
	}

	/**
	 * ネーミングコンテキストから指定された名前のオブジェクトをLookupする
	 * @param dsName データソース名
	 * @return Lookupで取得したデータソース
	 * @exception UroborosqlRuntimeException データソースが見つからなかった場合
	 */
	private static DataSource getNewDataSource(final String dsName) {
		try {
			Context context = new InitialContext();
			return (DataSource) context.lookup(dsName);
		} catch (NamingException ex) {
			throw new UroborosqlRuntimeException("DataSource[" + dsName + "] can not be acquired.", ex);
		}
	}

	/**
	 * データソース名の取得
	 *
	 * @return データソース名
	 */
	public String getDataSourceName() {
		return dataSourceName;
	}

	/**
	 * データソース名の設定
	 *
	 * @param dataSourceName データソース名
	 */
	public void setDataSourceName(final String dataSourceName) {
		this.dataSourceName = dataSourceName;
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDataSourceName(String)}
	 * で指定したデータソースに対するAutoCommitオプションの指定
	 *
	 * @param autoCommit
	 *            AutoCommitを行う場合は<code>true</code>
	 */
	public void setDefaultAutoCommit(final boolean autoCommit) {
		Map<String, String> props = getConnPropsByDsName(getDataSourceName());
		props.put(PROPS_AUTO_COMMIT, Boolean.toString(autoCommit));
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDataSourceName(String)}
	 * で指定したデータソースに対するAutoCommitオプションの取得
	 *
	 * @return AutoCommitを行う場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean getDefaultAutoCommit() {
		return getAutoCommit(getDataSourceName());
	}

	/**
	 * dsNameで指定したデータソースに対するAutoCommitオプションの取得
	 *
	 * @param dsName
	 *            データソース名
	 * @return AutoCommitを行う場合は<code>true</code>. 初期値は<code>false</code>
	 */
	protected boolean getAutoCommit(final String dsName) {
		Map<String, String> props = getConnPropsByDsName(dsName);
		return Boolean.parseBoolean(props.get(PROPS_AUTO_COMMIT));
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDataSourceName(String)}
	 * で指定したデータソースに対するReadOnlyオプションの指定
	 *
	 * @param readOnly
	 *            readOnlyを指定する場合は<code>true</code>
	 */
	public void setDefaultReadOnly(final boolean readOnly) {
		Map<String, String> props = getConnPropsByDsName(getDataSourceName());
		props.put(PROPS_READ_ONLY, Boolean.toString(readOnly));
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDataSourceName(String)}
	 * で指定したデータソースに対するReadOnlyオプションの指定
	 *
	 * @return readOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean getDefaultReadOnly() {
		return getReadOnly(getDataSourceName());
	}

	/**
	 * dsNameで指定したデータソースに対するReadOnlyオプションの指定
	 *
	 * @param dsName
	 *            データソース名
	 * @return readOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	protected boolean getReadOnly(final String dsName) {
		Map<String, String> props = getConnPropsByDsName(dsName);
		return Boolean.parseBoolean(props.get(PROPS_READ_ONLY));
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDataSourceName(String)}
	 * で指定したデータソースに対するtransactionIsolationオプションの指定
	 *
	 * @see Connection#TRANSACTION_READ_UNCOMMITTED
	 * @see Connection#TRANSACTION_READ_COMMITTED
	 * @see Connection#TRANSACTION_REPEATABLE_READ
	 * @see Connection#TRANSACTION_SERIALIZABLE
	 *
	 * @param transactionIsolation transactionIsolationオプション
	 *
	 */
	public void setDefaultTransactionIsolation(final int transactionIsolation) {
		if (Connection.TRANSACTION_READ_UNCOMMITTED == transactionIsolation
				|| Connection.TRANSACTION_READ_COMMITTED == transactionIsolation
				|| Connection.TRANSACTION_REPEATABLE_READ == transactionIsolation
				|| Connection.TRANSACTION_SERIALIZABLE == transactionIsolation) {
			Map<String, String> props = getConnPropsByDsName(getDataSourceName());
			props.put(PROPS_TRANSACTION_ISOLATION, String.valueOf(transactionIsolation));
		} else {
			throw new IllegalArgumentException("Unsupported level [" + transactionIsolation + "]");
		}
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDataSourceName(String)}
	 * で指定したデータソースに対するtransactionIsolationオプションの取得
	 *
	 * @return readOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public int getDefaultTransactionIsolation() {
		return getTransactionIsolation(getDataSourceName());
	}

	/**
	 * dsNameで指定したデータソースに対するtransactionIsolationオプションの指定
	 *
	 * @param dsName
	 *            データソース名
	 * @return readOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	protected int getTransactionIsolation(final String dsName) {
		Map<String, String> props = getConnPropsByDsName(dsName);
		String val = props.get(PROPS_TRANSACTION_ISOLATION);
		return val == null ? -1 : Integer.parseInt(val);
	}

	/**
	 * データソース名を指定したプロパティの取得
	 *
	 * @param dsName
	 *            データソース名
	 *
	 * @return　データソースに紐付くプロパティ
	 */
	protected Map<String, String> getConnPropsByDsName(final String dsName) {
		return connProps.computeIfAbsent(dsName, k -> new ConcurrentHashMap<>());
	}

}
