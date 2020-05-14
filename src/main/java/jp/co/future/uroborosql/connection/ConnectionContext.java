package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.util.concurrent.ConcurrentHashMap;

public class ConnectionContext extends ConcurrentHashMap<String, Object> {
	public static String PROPS_AUTO_COMMIT = "autocommit";
	public static String PROPS_READ_ONLY = "readonly";
	public static String PROPS_TRANSACTION_ISOLATION = "transactionisolation";

	public ConnectionContext() {
		super();
	}

	public ConnectionContext(final ConnectionContext connectionContext) {
		super(connectionContext);
	}

	/**
	 * 指定したDB接続情報からAutoCommitオプションを取得
	 *
	 * @return AutoCommitを行う場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean autoCommit() {
		return (boolean) getOrDefault(PROPS_AUTO_COMMIT, false);
	}

	/**
	 * 指定したDB接続情報にAutoCommitオプションを指定
	 *
	 * @param <T> {@link ConnectionContext}の具象型
	 * @param autoCommit AutoCommitを行う場合は<code>true</code>
	 * @return {@link ConnectionContext}
	 */
	@SuppressWarnings("unchecked")
	public <T extends ConnectionContext> T autoCommit(final boolean autoCommit) {
		put(PROPS_AUTO_COMMIT, autoCommit);
		return (T) this;
	}

	/**
	 * 指定したDB接続情報からReadOnlyオプションを取得
	 *
	 * @param connProps DB接続情報
	 * @return ReadOnlyの場合は<code>true</code>. 初期値は<code>false</code>
	 */
	public boolean readOnly() {
		return (boolean) getOrDefault(PROPS_READ_ONLY, false);
	}

	/**
	 * 指定したDB接続情報にReadOnlyオプションを指定
	 *
	 * @param <T> {@link ConnectionContext}の具象型
	 * @param readOnly readOnlyを指定する場合は<code>true</code>
	 * @return {@link ConnectionContext}
	 */
	@SuppressWarnings("unchecked")
	public <T extends ConnectionContext> T readOnly(final boolean readOnly) {
		put(PROPS_READ_ONLY, readOnly);
		return (T) this;
	}

	/**
	 * 指定したDB接続情報からtransactionIsolationオプションを取得
	 *
	 * @see Connection#TRANSACTION_READ_UNCOMMITTED
	 * @see Connection#TRANSACTION_READ_COMMITTED
	 * @see Connection#TRANSACTION_REPEATABLE_READ
	 * @see Connection#TRANSACTION_SERIALIZABLE
	 *
	 * @return transactionIsolationの指定がない場合は<code>-1</code>. 指定がある場合はその値
	 */
	public int transactionIsolation() {
		return (int) getOrDefault(PROPS_TRANSACTION_ISOLATION, -1);
	}

	/**
	 * 指定したDB接続情報にtransactionIsolationオプションを指定
	 *
	 * @see Connection#TRANSACTION_READ_UNCOMMITTED
	 * @see Connection#TRANSACTION_READ_COMMITTED
	 * @see Connection#TRANSACTION_REPEATABLE_READ
	 * @see Connection#TRANSACTION_SERIALIZABLE
	 *
	 * @param <T> {@link ConnectionContext}の具象型
	 * @param transactionIsolation transactionIsolationオプション
	 * @return {@link ConnectionContext}
	 */
	@SuppressWarnings("unchecked")
	public <T extends ConnectionContext> T transactionIsolation(final int transactionIsolation) {
		if (Connection.TRANSACTION_READ_UNCOMMITTED == transactionIsolation
				|| Connection.TRANSACTION_READ_COMMITTED == transactionIsolation
				|| Connection.TRANSACTION_REPEATABLE_READ == transactionIsolation
				|| Connection.TRANSACTION_SERIALIZABLE == transactionIsolation) {
			put(PROPS_TRANSACTION_ISOLATION, transactionIsolation);
		} else {
			throw new IllegalArgumentException("Unsupported level [" + transactionIsolation + "]");
		}
		return (T) this;
	}
}
