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

	/** データソース名とデータソースのマップ */
	private final Map<String, DataSource> datasources = new ConcurrentHashMap<>();

	/** デフォルトデータソース用のDB接続情報 */
	private final DataSourceConnectionContext defaultConnectionContext;

	/**
	 * コンストラクタ。
	 */
	public DataSourceConnectionSupplierImpl() {
		this(ConnectionContextBuilder.dataSource());
	}

	/**
	 * コンストラクタ。
	 *
	 * @param defaultDataSourceName 取得するコネクションのデータソース名
	 */
	@Deprecated
	public DataSourceConnectionSupplierImpl(final String defaultDataSourceName) {
		this(ConnectionContextBuilder.dataSource(defaultDataSourceName));
	}

	/**
	 * コンストラクタ。
	 *
	 * @param connectionContext DB接続情報
	 */
	public DataSourceConnectionSupplierImpl(final DataSourceConnectionContext connectionContext) {
		this.defaultConnectionContext = connectionContext;
	}

	/**
	 * コンストラクタ。
	 *
	 * @param defaultDataSource 取得するコネクションのデータソース
	 */
	public DataSourceConnectionSupplierImpl(final DataSource defaultDataSource) {
		this();
		this.datasources.put(this.defaultConnectionContext.dataSourceName(), defaultDataSource);

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
		if (!(ctx instanceof DataSourceConnectionContext)) {
			throw new IllegalArgumentException("ctx must be of type DataSourceConnectionContext.");
		}
		String datasourceName = ((DataSourceConnectionContext) ctx).dataSourceName();
		try {
			DataSource ds = datasources.computeIfAbsent(datasourceName,
					DataSourceConnectionSupplierImpl::getNewDataSource);
			final Connection connection;
			synchronized (ds) {
				connection = new MetadataCachedConnectionWrapper(ds.getConnection(), ctx.cacheSchemaName());
			}
			if (ctx.autoCommit() != connection.getAutoCommit()) {
				connection.setAutoCommit(ctx.autoCommit());
			}
			if (ctx.readOnly() != connection.isReadOnly()) {
				connection.setReadOnly(ctx.readOnly());
			}
			int transactionIsolation = ctx.transactionIsolation();
			if (transactionIsolation > 0 && transactionIsolation != connection.getTransactionIsolation()) {
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
		return defaultConnectionContext.dataSourceName();
	}

	/**
	 * デフォルトデータソース名の設定
	 *
	 * @param defaultDataSourceName デフォルトデータソース名
	 */
	public void setDefaultDataSourceName(final String defaultDataSourceName) {
		defaultConnectionContext.dataSourceName(defaultDataSourceName);
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するAutoCommitオプションの取得
	 *
	 * @return AutoCommitを行う場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean isDefaultAutoCommit() {
		return defaultConnectionContext.autoCommit();
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するAutoCommitオプションの指定
	 *
	 * @param autoCommit AutoCommitを行う場合は<code>true</code>
	 */
	public void setDefaultAutoCommit(final boolean autoCommit) {
		defaultConnectionContext.autoCommit(autoCommit);
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するReadOnlyオプションを指定
	 *
	 * @return readOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean isDefaultReadOnly() {
		return defaultConnectionContext.readOnly();
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するReadOnlyオプションを指定
	 *
	 * @param readOnly readOnlyを指定する場合は<code>true</code>
	 */
	public void setDefaultReadOnly(final boolean readOnly) {
		defaultConnectionContext.readOnly(readOnly);
	}

	/**
	 * {@link DataSourceConnectionSupplierImpl#setDefaultDataSourceName(String)}
	 * で指定したデータソースに対するtransactionIsolationオプションの取得
	 *
	 * @return transactionIsolationの指定がない場合は<code>-1</code>. 指定がある場合はその値
	 */
	public int getDefaultTransactionIsolation() {
		return defaultConnectionContext.transactionIsolation();
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
		defaultConnectionContext.transactionIsolation(transactionIsolation);
	}

}
