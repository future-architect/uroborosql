/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;
import java.util.Map;

import jp.co.future.uroborosql.exception.UroborosqlSQLException;

/**
 * JDBCコネクション提供インターフェース
 *
 * @author H.Sugimoto
 */
public interface ConnectionSupplier {
	String PROPS_AUTO_COMMIT = "autocommit";
	String PROPS_READ_ONLY = "readonly";
	String PROPS_TRANSACTION_ISOLATION = "transactionisolation";

	/**
	 * コネクション取得。
	 * @return コネクション
	 */
	Connection getConnection();

	/**
	 * コネクション取得。
	 * @param props DB接続に使用するプロパティ
	 * @return コネクション
	 */
	Connection getConnection(Map<String, String> props);

	/**
	 * 接続しているDBプロダクト名+ バージョンを取得する
	 *
	 * @return DatabaseName + "-" + DatabaseVersion
	 */
	default String getDatabaseName() {
		Connection conn = null;
		try {
			conn = getConnection();
			DatabaseMetaData metaData = conn.getMetaData();
			return metaData.getDatabaseProductName() + "-" + metaData.getDatabaseMajorVersion() + "."
					+ metaData.getDatabaseMinorVersion();
		} catch (SQLException ex) {
			throw new UroborosqlSQLException(ex);
		} finally {
			try {
				if (conn != null) {
					conn.close();
				}
			} catch (SQLException ex) {
				throw new UroborosqlSQLException(ex);
			}
		}
	}

	/**
	 * 指定したDB接続情報からAutoCommitオプションを取得
	 *
	 * @param connProps DB接続情報
	 * @return AutoCommitを行う場合は<code>true</code>. 初期値は<code>false</code>
	 */
	default boolean isAutoCommit(final Map<String, String> connProps) {
		return Boolean.parseBoolean(connProps.get(PROPS_AUTO_COMMIT));
	}

	/**
	 * 指定したDB接続情報にAutoCommitオプションを指定
	 *
	 * @param connProps DB接続情報
	 * @param autoCommit AutoCommitを行う場合は<code>true</code>
	 */
	default void setAutoCommit(final Map<String, String> connProps, final boolean autoCommit) {
		connProps.put(PROPS_AUTO_COMMIT, Boolean.toString(autoCommit));
	}

	/**
	 * 指定したDB接続情報からReadOnlyオプションを取得
	 *
	 * @param connProps DB接続情報
	 * @return ReadOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	default boolean isReadOnly(final Map<String, String> connProps) {
		return Boolean.parseBoolean(connProps.get(PROPS_READ_ONLY));
	}

	/**
	 * 指定したDB接続情報にReadOnlyオプションを指定
	 *
	 * @param connProps DB接続情報
	 * @param readOnly readOnlyを指定する場合は<code>true</code>
	 */
	default void setReadOnly(final Map<String, String> connProps, final boolean readOnly) {
		connProps.put(PROPS_READ_ONLY, Boolean.toString(readOnly));
	}

	/**
	 * 指定したDB接続情報からtransactionIsolationオプションを取得
	 *
	 * @see Connection#TRANSACTION_READ_UNCOMMITTED
	 * @see Connection#TRANSACTION_READ_COMMITTED
	 * @see Connection#TRANSACTION_REPEATABLE_READ
	 * @see Connection#TRANSACTION_SERIALIZABLE
	 *
	 * @param connProps DB接続情報
	 * @return transactionIsolationの指定がない場合は<code>-1</code>. 指定がある場合はその値
	 */
	default int getTransactionIsolation(final Map<String, String> connProps) {
		String val = connProps.get(PROPS_TRANSACTION_ISOLATION);
		if (val != null) {
			try {
				int isolation = Integer.parseInt(val);
				if (isolation == Connection.TRANSACTION_READ_UNCOMMITTED ||
						isolation == Connection.TRANSACTION_READ_COMMITTED ||
						isolation == Connection.TRANSACTION_REPEATABLE_READ ||
						isolation == Connection.TRANSACTION_SERIALIZABLE) {
					return isolation;
				}
			} catch (NumberFormatException ex) {
				throw new IllegalArgumentException(ex);
			}
		}
		return -1;
	}

	/**
	 * 指定したDB接続情報にtransactionIsolationオプションを指定
	 *
	 * @see Connection#TRANSACTION_READ_UNCOMMITTED
	 * @see Connection#TRANSACTION_READ_COMMITTED
	 * @see Connection#TRANSACTION_REPEATABLE_READ
	 * @see Connection#TRANSACTION_SERIALIZABLE
	 *
	 * @param connProps DB接続情報
	 * @param transactionIsolation transactionIsolationオプション
	 */
	default void setTransactionIsolation(final Map<String, String> connProps, final int transactionIsolation) {
		if (Connection.TRANSACTION_READ_UNCOMMITTED == transactionIsolation
				|| Connection.TRANSACTION_READ_COMMITTED == transactionIsolation
				|| Connection.TRANSACTION_REPEATABLE_READ == transactionIsolation
				|| Connection.TRANSACTION_SERIALIZABLE == transactionIsolation) {
			connProps.put(PROPS_TRANSACTION_ISOLATION, String.valueOf(transactionIsolation));
		} else {
			throw new IllegalArgumentException("Unsupported level [" + transactionIsolation + "]");
		}
	}

}
