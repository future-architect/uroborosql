/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
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
import jp.co.future.uroborosql.exception.UroborosqlSQLException;

/**
 * データソースを使用したコネクション供給クラス<br>
 * 指定されたデータソースからコネクションを取得する
 *
 * @author H.Sugimoto
 */
public class DataSourceConnectionSupplierImpl implements ConnectionSupplier {

	/** プロパティ：データソース名 */
	public static final String PROPS_DATASOURCE_NAME = "datasource_name";

	/** デフォルトデータソース名 */
	public static final String DEFAULT_DATASOURCE_NAME = "java:comp/env/jdbc/default_datasource";

	/** データソース名とデータソースのマップ */
	private final Map<String, DataSource> datasources = new ConcurrentHashMap<>();

	/** デフォルトデータソース用のDB接続情報 */
	private final Map<String, String> defaultConnProps = new ConcurrentHashMap<>();

	/**
	 * コンストラクタ。
	 */
	public DataSourceConnectionSupplierImpl() {
		this(DEFAULT_DATASOURCE_NAME);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param defaultDataSourceName 取得するコネクションのデータソース名
	 */
	public DataSourceConnectionSupplierImpl(final String defaultDataSourceName) {
		setDefaultDataSourceName(defaultDataSourceName);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param defaultDataSource 取得するコネクションのデータソース
	 */
	public DataSourceConnectionSupplierImpl(final DataSource defaultDataSource) {
		this();
		datasources.put(DEFAULT_DATASOURCE_NAME, defaultDataSource);

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
	public Connection getConnection(final Map<String, String> connProps) {
		String datasourceName = connProps.get(PROPS_DATASOURCE_NAME);
		try {
			DataSource ds = datasources.computeIfAbsent(datasourceName,
					DataSourceConnectionSupplierImpl::getNewDataSource);
			final Connection connection;
			synchronized (ds) {
				connection = ds.getConnection();
			}
			connection.setAutoCommit(isAutoCommit(connProps));
			connection.setReadOnly(isReadOnly(connProps));
			int transactionIsolation = getTransactionIsolation(connProps);
			if (transactionIsolation > 0) {
				connection.setTransactionIsolation(transactionIsolation);
			}
			return connection;
		} catch (SQLException ex) {
			throw new UroborosqlSQLException("Connection[" + datasourceName + "] can not be acquired.", ex);
		}
	}

	/**
	 * ネーミングコンテキストから指定された名前のオブジェクトをLookupする
	 * @param dataSourceName データソース名
	 * @return Lookupで取得したデータソース
	 * @exception UroborosqlRuntimeException データソースが見つからなかった場合
	 */
	private static DataSource getNewDataSource(final String dataSourceName) {
		try {
			Context context = new InitialContext();
			return (DataSource) context.lookup(dataSourceName);
		} catch (NamingException ex) {
			throw new UroborosqlRuntimeException("DataSource[" + dataSourceName + "] can not be acquired.", ex);
		}
	}

	/**
	 * デフォルトデータソース名の取得
	 *
	 * @return デフォルトデータソース名
	 */
	public String getDefaultDataSourceName() {
		return defaultConnProps.get(PROPS_DATASOURCE_NAME);
	}

	/**
	 * デフォルトデータソース名の設定
	 *
	 * @param defaultDataSourceName デフォルトデータソース名
	 */
	public void setDefaultDataSourceName(final String defaultDataSourceName) {
		if (defaultDataSourceName == null) {
			throw new IllegalArgumentException("defaultDataSourceName is null.");
		}
		defaultConnProps.put(PROPS_DATASOURCE_NAME, defaultDataSourceName);
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するAutoCommitオプションの取得
	 *
	 * @return AutoCommitを行う場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean isDefaultAutoCommit() {
		return isAutoCommit(defaultConnProps);
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するAutoCommitオプションの指定
	 *
	 * @param autoCommit AutoCommitを行う場合は<code>true</code>
	 */
	public void setDefaultAutoCommit(final boolean autoCommit) {
		setAutoCommit(defaultConnProps, autoCommit);
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するReadOnlyオプションを指定
	 *
	 * @return readOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean isDefaultReadOnly() {
		return isReadOnly(defaultConnProps);
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するReadOnlyオプションを指定
	 *
	 * @param readOnly readOnlyを指定する場合は<code>true</code>
	 */
	public void setDefaultReadOnly(final boolean readOnly) {
		setReadOnly(defaultConnProps, readOnly);
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するtransactionIsolationオプションの取得
	 *
	 * @return transactionIsolationの指定がない場合は<code>-1</code>. 指定がある場合はその値
	 */
	public int getDefaultTransactionIsolation() {
		return getTransactionIsolation(defaultConnProps);
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するtransactionIsolationオプションを指定
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
		setTransactionIsolation(defaultConnProps, transactionIsolation);
	}

}
