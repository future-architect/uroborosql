package jp.co.future.uroborosql;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.store.SqlManager;

/**
 * Sql実行クラスのファクトリクラス。
 *
 * @author H.Sugimoto
 */
public class SqlAgentFactoryImpl implements SqlAgentFactory {
	/** プロパティ：例外発生時のログ出力を行うかどうか。デフォルトは<code>true</code> */
	public static final String PROPS_KEY_OUTPUT_EXCEPTION_LOG = "outputExceptionLog";

	/** コネクションサプライヤ */
	private ConnectionSupplier connectionSupplier;

	/** SQL管理クラス */
	private SqlManager sqlManager;

	/** SqlFilter管理クラス */
	private SqlFilterManager sqlFilterManager;

	/** デフォルト値を保持するプロパティ */
	private final Map<String, String> defaultProps = new HashMap<>();

	/**
	 * コンストラクタ。
	 */
	public SqlAgentFactoryImpl() {
		this(null, null, null);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param connectionSupplier コネクション供給クラス
	 * @param sqlManager SQL管理クラス
	 * @param sqlFilterManager SQLフィルタ管理クラス
	 */
	public SqlAgentFactoryImpl(final ConnectionSupplier connectionSupplier, final SqlManager sqlManager,
			final SqlFilterManager sqlFilterManager) {
		this.connectionSupplier = connectionSupplier;
		this.sqlManager = sqlManager;
		this.sqlFilterManager = sqlFilterManager;
		getDefaultProps().put(PROPS_KEY_OUTPUT_EXCEPTION_LOG, Boolean.TRUE.toString());
		getDefaultProps().put(PROPS_KEY_REMOVE_TERMINATOR, Boolean.TRUE.toString());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#createSqlAgent()
	 */
	@Override
	public SqlAgent createSqlAgent() {
		return new SqlAgentImpl(getConnectionSupplier(), getSqlManager(), getSqlFilterManager(), getDefaultProps());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getConnectionSupplier()
	 */
	@Override
	public ConnectionSupplier getConnectionSupplier() {
		return connectionSupplier;
	}

	/**
	 * コネクション供給クラスを設定します。
	 *
	 * @param connectionSupplier コネクション供給クラス
	 */
	public void setConnectionSupplier(final ConnectionSupplier connectionSupplier) {
		this.connectionSupplier = connectionSupplier;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getSqlManager()
	 */
	@Override
	public SqlManager getSqlManager() {
		return sqlManager;
	}

	/**
	 * SQL管理クラスを設定します。
	 *
	 * @param sqlManager SQL管理クラス
	 */
	public void setSqlManager(final SqlManager sqlManager) {
		this.sqlManager = sqlManager;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getSqlFilterManager()
	 */
	@Override
	public SqlFilterManager getSqlFilterManager() {
		return sqlFilterManager;
	}

	/**
	 * SQLフィルタ管理クラスを設定します。
	 *
	 * @param sqlFilterManager SQLフィルタ管理クラス
	 */
	public void setSqlFilterManager(final SqlFilterManager sqlFilterManager) {
		this.sqlFilterManager = sqlFilterManager;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#isOutputExceptionLog()
	 */
	@Override
	public boolean isOutputExceptionLog() {
		return Boolean.parseBoolean(getDefaultProps().get(PROPS_KEY_OUTPUT_EXCEPTION_LOG));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setOutputExceptionLog(boolean)
	 */
	@Override
	public void setOutputExceptionLog(final boolean outputExceptionLog) {
		getDefaultProps().put(PROPS_KEY_OUTPUT_EXCEPTION_LOG, Boolean.toString(outputExceptionLog));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#isRemoveTerminator()
	 */
	@Override
	public boolean isRemoveTerminator() {
		return Boolean.parseBoolean(getDefaultProps().get(PROPS_KEY_REMOVE_TERMINATOR));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setRemoveTerminator(boolean)
	 */
	@Override
	public void setRemoveTerminator(final boolean removeTerminator) {
		getDefaultProps().put(PROPS_KEY_REMOVE_TERMINATOR, Boolean.toString(removeTerminator));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getFetchSize()
	 */
	@Override
	public int getFetchSize() {
		return Integer.parseInt(getDefaultProps().getOrDefault(PROPS_KEY_FETCH_SIZE, "-1"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setFetchSize(int)
	 */
	@Override
	public void setFetchSize(final int fetchSize) {
		getDefaultProps().put(PROPS_KEY_FETCH_SIZE, String.valueOf(fetchSize));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getQueryTimeout()
	 */
	@Override
	public int getQueryTimeout() {
		return Integer.parseInt(getDefaultProps().getOrDefault(PROPS_KEY_QUERY_TIMEOUT, "-1"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setQueryTimeout(int)
	 */
	@Override
	public void setQueryTimeout(final int queryTimeout) {
		getDefaultProps().put(PROPS_KEY_QUERY_TIMEOUT, String.valueOf(queryTimeout));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getSqlRetryCodeList()
	 */
	@Override
	public List<String> getSqlRetryCodeList() {
		String codes = getDefaultProps().get(PROPS_KEY_SQL_RETRY_CODES);
		if (codes == null) {
			return Collections.emptyList();
		} else {
			return Arrays.asList(codes.split(","));
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setSqlRetryCodeList(java.util.List)
	 */
	@Override
	public void setSqlRetryCodeList(final List<String> sqlRetryCodeList) {
		if (sqlRetryCodeList != null && !sqlRetryCodeList.isEmpty()) {
			getDefaultProps().put(PROPS_KEY_SQL_RETRY_CODES, String.join(",", sqlRetryCodeList));
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getDefaultMaxRetryCount()
	 */
	@Override
	public int getDefaultMaxRetryCount() {
		return Integer.parseInt(getDefaultProps().getOrDefault(PROPS_KEY_DEFAULT_MAX_RETRY_COUNT, "0"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setDefaultMaxRetryCount(int)
	 */
	@Override
	public void setDefaultMaxRetryCount(final int defaultMaxRetryCount) {
		getDefaultProps().put(PROPS_KEY_DEFAULT_MAX_RETRY_COUNT, String.valueOf(defaultMaxRetryCount));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getDefaultSqlRetryWaitTime()
	 */
	@Override
	public int getDefaultSqlRetryWaitTime() {
		return Integer.parseInt(getDefaultProps().getOrDefault(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME, "0"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setDefaultSqlRetryWaitTime(int)
	 */
	@Override
	public void setDefaultSqlRetryWaitTime(final int defaultSqlRetryWaitTime) {
		getDefaultProps().put(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME, String.valueOf(defaultSqlRetryWaitTime));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getSqlIdKeyName()
	 */
	@Override
	public String getSqlIdKeyName() {
		return getDefaultProps().getOrDefault(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME, "_SQL_ID_");
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setSqlIdKeyName(java.lang.String)
	 */
	@Override
	public void setSqlIdKeyName(final String sqlIdKeyName) {
		getDefaultProps().put(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME, sqlIdKeyName);
	}

	/**
	 * defaultProps を取得します
	 *
	 * @return defaultProps
	 */
	protected Map<String, String> getDefaultProps() {
		return defaultProps;
	}

}
