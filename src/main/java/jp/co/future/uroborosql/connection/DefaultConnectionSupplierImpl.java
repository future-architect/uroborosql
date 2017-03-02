package jp.co.future.uroborosql.connection;

import java.sql.Connection;

/**
 * デフォルトコネクション供給クラス<br>
 * コネクションを保持して返すだけの実装。<br>
 * @author H.Sugimoto
 */
public class DefaultConnectionSupplierImpl implements ConnectionSupplier {
	/**  コネクション */
	private final Connection connection;

	/**
	 * コンストラクタ。
	 * @param connection コネクション
	 */
	public DefaultConnectionSupplierImpl(final Connection connection) {
		this.connection = connection;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection()
	 */
	@Override
	public Connection getConnection() {
		return connection;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.connection.ConnectionSupplier#getConnection(java.lang.String)
	 */
	@Override
	public Connection getConnection(final String alias) {
		return connection;
	}

}
