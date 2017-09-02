package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.sql.SQLException;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * ConnectionSupplierから一度Connectionを取得したらクローズされるまで使いまわすConnectionManager
 *
 * @author ota
 */
public class DefaultConnectionManager implements ConnectionManager {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(DefaultConnectionManager.class);

	/** コネクション供給クラス */
	private final ConnectionSupplier connectionSupplier;

	/** 保持するコネクション */
	private Connection conn;

	/**
	 * コンストラクタ
	 *
	 * @param connectionSupplier コネクション供給クラス
	 */
	public DefaultConnectionManager(final ConnectionSupplier connectionSupplier) {
		this.connectionSupplier = connectionSupplier;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#getConnection()
	 */
	@Override
	public Connection getConnection() {
		if (conn == null) {
			conn = connectionSupplier.getConnection();
		}
		return conn;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#getConnection(java.lang.String)
	 */
	@Override
	public Connection getConnection(final String alias) {
		if (conn == null) {
			conn = connectionSupplier.getConnection(alias);
		}
		return conn;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#close()
	 */
	@Override
	public void close() {
		if (conn != null) {
			LOG.trace("Close connection. conn[{}], hashCode[{}]", conn, conn.hashCode());
			try {
				conn.close();
			} catch (SQLException e) {
				throw new UroborosqlRuntimeException(e);
			}
			conn = null;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#commit()
	 */
	@Override
	public void commit() throws SQLException {
		if (conn != null) {
			conn.commit();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionManager#rollback()
	 */
	@Override
	public void rollback() throws SQLException {
		if (conn != null) {
			conn.rollback();
		}
	}
}