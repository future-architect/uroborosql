/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.Array;
import java.sql.Blob;
import java.sql.CallableStatement;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.NClob;
import java.sql.PreparedStatement;
import java.sql.SQLClientInfoException;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.SQLXML;
import java.sql.Savepoint;
import java.sql.Statement;
import java.sql.Struct;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Executor;

/**
 * Closeを無視するConnectionを提供するためのWrapper
 *
 * @author H.Sugimoto
 */
public class CloseIgnoringConnectionWrapper implements Connection {
	private final Connection original;

	/**
	 * コンストラクタ
	 *
	 * @param original 元となるコネクション
	 */
	public CloseIgnoringConnectionWrapper(final Connection original) {
		this.original = original;
	}

	@Override
	public Statement createStatement() throws SQLException {
		return original.createStatement();
	}

	@Override
	public PreparedStatement prepareStatement(final String sql) throws SQLException {
		return original.prepareStatement(sql);
	}

	@Override
	public CallableStatement prepareCall(final String sql) throws SQLException {
		return original.prepareCall(sql);
	}

	@Override
	public String nativeSQL(final String sql) throws SQLException {
		return original.nativeSQL(sql);
	}

	@Override
	public void setAutoCommit(final boolean autoCommit) throws SQLException {
		original.setAutoCommit(autoCommit);
	}

	@Override
	public boolean getAutoCommit() throws SQLException {
		return original.getAutoCommit();
	}

	@Override
	public void commit() throws SQLException {
		original.commit();
	}

	@Override
	public void rollback() throws SQLException {
		original.rollback();
	}

	@Override
	public void close() throws SQLException {
		// do nothing
	}

	@Override
	public boolean isClosed() throws SQLException {
		return original.isClosed();
	}

	@Override
	public DatabaseMetaData getMetaData() throws SQLException {
		return original.getMetaData();
	}

	@Override
	public void setReadOnly(final boolean readOnly) throws SQLException {
		original.setReadOnly(readOnly);
	}

	@Override
	public boolean isReadOnly() throws SQLException {
		return original.isReadOnly();
	}

	@Override
	public void setCatalog(final String catalog) throws SQLException {
		original.setCatalog(catalog);
	}

	@Override
	public String getCatalog() throws SQLException {
		return original.getCatalog();
	}

	@Override
	public void setTransactionIsolation(final int level) throws SQLException {
		original.setTransactionIsolation(level);
	}

	@Override
	public int getTransactionIsolation() throws SQLException {
		return original.getTransactionIsolation();
	}

	@Override
	public SQLWarning getWarnings() throws SQLException {
		return original.getWarnings();
	}

	@Override
	public void clearWarnings() throws SQLException {
		original.clearWarnings();
	}

	@Override
	public Statement createStatement(final int resultSetType, final int resultSetConcurrency) throws SQLException {
		return original.createStatement(resultSetType, resultSetConcurrency);
	}

	@Override
	public PreparedStatement prepareStatement(final String sql, final int resultSetType, final int resultSetConcurrency)
			throws SQLException {
		return original.prepareStatement(sql, resultSetType, resultSetConcurrency);
	}

	@Override
	public CallableStatement prepareCall(final String sql, final int resultSetType, final int resultSetConcurrency)
			throws SQLException {
		return original.prepareCall(sql, resultSetType, resultSetConcurrency);
	}

	@Override
	public Map<String, Class<?>> getTypeMap() throws SQLException {
		return original.getTypeMap();
	}

	@Override
	public void setTypeMap(final Map<String, Class<?>> map) throws SQLException {
		original.setTypeMap(map);
	}

	@Override
	public void setHoldability(final int holdability) throws SQLException {
		original.setHoldability(holdability);
	}

	@Override
	public int getHoldability() throws SQLException {
		return original.getHoldability();
	}

	@Override
	public Savepoint setSavepoint() throws SQLException {
		return original.setSavepoint();
	}

	@Override
	public Savepoint setSavepoint(final String name) throws SQLException {
		return original.setSavepoint(name);
	}

	@Override
	public void rollback(final Savepoint savepoint) throws SQLException {
		original.rollback(savepoint);
	}

	@Override
	public void releaseSavepoint(final Savepoint savepoint) throws SQLException {
		original.releaseSavepoint(savepoint);
	}

	@Override
	public Statement createStatement(final int resultSetType, final int resultSetConcurrency,
			final int resultSetHoldability)
			throws SQLException {
		return original.createStatement(resultSetType, resultSetConcurrency, resultSetHoldability);
	}

	@Override
	public PreparedStatement prepareStatement(final String sql, final int resultSetType, final int resultSetConcurrency,
			final int resultSetHoldability) throws SQLException {
		return original.prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
	}

	@Override
	public CallableStatement prepareCall(final String sql, final int resultSetType, final int resultSetConcurrency,
			final int resultSetHoldability) throws SQLException {
		return original.prepareCall(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
	}

	@Override
	public PreparedStatement prepareStatement(final String sql, final int autoGeneratedKeys) throws SQLException {
		return original.prepareStatement(sql, autoGeneratedKeys);
	}

	@Override
	public PreparedStatement prepareStatement(final String sql, final int[] columnIndexes) throws SQLException {
		return original.prepareStatement(sql, columnIndexes);
	}

	@Override
	public PreparedStatement prepareStatement(final String sql, final String[] columnNames) throws SQLException {
		return original.prepareStatement(sql, columnNames);
	}

	@Override
	public Clob createClob() throws SQLException {
		return original.createClob();
	}

	@Override
	public Blob createBlob() throws SQLException {
		return original.createBlob();
	}

	@Override
	public NClob createNClob() throws SQLException {
		return original.createNClob();
	}

	@Override
	public SQLXML createSQLXML() throws SQLException {
		return original.createSQLXML();
	}

	@Override
	public boolean isValid(final int timeout) throws SQLException {
		return original.isValid(timeout);
	}

	@Override
	public void setClientInfo(final String name, final String value) throws SQLClientInfoException {
		original.setClientInfo(name, value);
	}

	@Override
	public void setClientInfo(final Properties properties) throws SQLClientInfoException {
		original.setClientInfo(properties);
	}

	@Override
	public String getClientInfo(final String name) throws SQLException {
		return original.getClientInfo(name);
	}

	@Override
	public Properties getClientInfo() throws SQLException {
		return original.getClientInfo();
	}

	@Override
	public Array createArrayOf(final String typeName, final Object[] elements) throws SQLException {
		return original.createArrayOf(typeName, elements);
	}

	@Override
	public Struct createStruct(final String typeName, final Object[] attributes) throws SQLException {
		return original.createStruct(typeName, attributes);
	}

	@Override
	public void setSchema(final String schema) throws SQLException {
		original.setSchema(schema);
	}

	@Override
	public String getSchema() throws SQLException {
		return original.getSchema();
	}

	@Override
	public void abort(final Executor executor) throws SQLException {
		original.abort(executor);
	}

	@Override
	public void setNetworkTimeout(final Executor executor, final int milliseconds) throws SQLException {
		original.setNetworkTimeout(executor, milliseconds);
	}

	@Override
	public int getNetworkTimeout() throws SQLException {
		return original.getNetworkTimeout();
	}

	@Override
	public <T> T unwrap(final Class<T> iface) throws SQLException {
		return original.unwrap(iface);
	}

	@Override
	public boolean isWrapperFor(final Class<?> iface) throws SQLException {
		return original.isWrapperFor(iface);
	}
}
