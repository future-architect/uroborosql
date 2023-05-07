/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * Sql実行クラスのプロバイダクラス。
 *
 * @author H.Sugimoto
 */
public class SqlAgentProviderImpl implements SqlAgentProvider {
	/**
	 * プロパティ:例外発生時のログ出力を行うかどうか。
	 * デフォルトは<code>true</code>
	 */
	public static final String PROPS_KEY_OUTPUT_EXCEPTION_LOG = "outputExceptionLog";

	/** デフォルト値を保持するプロパティ */
	protected final Map<String, String> settings = new HashMap<>();

	/** SqlConfig */
	private SqlConfig sqlConfig;

	/**
	 * コンストラクタ。
	 *
	 */
	public SqlAgentProviderImpl() {
		this(null);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param sqlConfig SQL設定管理クラス
	 */
	public SqlAgentProviderImpl(final SqlConfig sqlConfig) {
		this.sqlConfig = sqlConfig;
		settings.put(PROPS_KEY_OUTPUT_EXCEPTION_LOG, Boolean.TRUE.toString());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#agent()
	 */
	@Override
	public SqlAgent agent() {
		return this.agent(null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#agent(jp.co.future.uroborosql.connection.ConnectionContext)
	 */
	@Override
	public SqlAgent agent(final ConnectionContext connectionContext) {
		if (sqlConfig == null) {
			throw new UroborosqlRuntimeException();
		}
		return new SqlAgentImpl(sqlConfig, settings, connectionContext);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfigAware#getSqlConfig()
	 */
	@Override
	public SqlConfig getSqlConfig() {
		return this.sqlConfig;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfigAware#setSqlConfig(jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public void setSqlConfig(final SqlConfig sqlConfig) {
		this.sqlConfig = sqlConfig;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#isOutputExceptionLog()
	 */
	@Override
	public boolean isOutputExceptionLog() {
		return Boolean.parseBoolean(settings.get(PROPS_KEY_OUTPUT_EXCEPTION_LOG));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setOutputExceptionLog(boolean)
	 */
	@Override
	public SqlAgentProvider setOutputExceptionLog(final boolean outputExceptionLog) {
		settings.put(PROPS_KEY_OUTPUT_EXCEPTION_LOG, Boolean.toString(outputExceptionLog));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#getFetchSize()
	 */
	@Override
	public int getFetchSize() {
		return Integer.parseInt(settings.getOrDefault(PROPS_KEY_FETCH_SIZE, "-1"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setFetchSize(int)
	 */
	@Override
	public SqlAgentProvider setFetchSize(final int fetchSize) {
		settings.put(PROPS_KEY_FETCH_SIZE, String.valueOf(fetchSize));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#getQueryTimeout()
	 */
	@Override
	public int getQueryTimeout() {
		return Integer.parseInt(settings.getOrDefault(PROPS_KEY_QUERY_TIMEOUT, "-1"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setQueryTimeout(int)
	 */
	@Override
	public SqlAgentProvider setQueryTimeout(final int queryTimeout) {
		settings.put(PROPS_KEY_QUERY_TIMEOUT, String.valueOf(queryTimeout));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#getSqlRetryCodeList()
	 */
	@Override
	public List<String> getSqlRetryCodeList() {
		var codes = settings.get(PROPS_KEY_SQL_RETRY_CODES);
		if (codes == null) {
			return List.of();
		} else {
			return List.of(codes.split(","));
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setSqlRetryCodeList(java.util.List)
	 */
	@Override
	public SqlAgentProvider setSqlRetryCodeList(final List<String> sqlRetryCodeList) {
		if (sqlRetryCodeList != null && !sqlRetryCodeList.isEmpty()) {
			settings.put(PROPS_KEY_SQL_RETRY_CODES, String.join(",", sqlRetryCodeList));
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#getDefaultMaxRetryCount()
	 */
	@Override
	public int getDefaultMaxRetryCount() {
		return Integer.parseInt(settings.getOrDefault(PROPS_KEY_DEFAULT_MAX_RETRY_COUNT, "0"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setDefaultMaxRetryCount(int)
	 */
	@Override
	public SqlAgentProvider setDefaultMaxRetryCount(final int defaultMaxRetryCount) {
		settings.put(PROPS_KEY_DEFAULT_MAX_RETRY_COUNT, String.valueOf(defaultMaxRetryCount));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#getDefaultSqlRetryWaitTime()
	 */
	@Override
	public int getDefaultSqlRetryWaitTime() {
		return Integer.parseInt(settings.getOrDefault(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME, "0"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setDefaultSqlRetryWaitTime(int)
	 */
	@Override
	public SqlAgentProvider setDefaultSqlRetryWaitTime(final int defaultSqlRetryWaitTime) {
		settings.put(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME, String.valueOf(defaultSqlRetryWaitTime));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#getSqlIdKeyName()
	 */
	@Override
	public String getSqlIdKeyName() {
		return settings.getOrDefault(PROPS_KEY_SQL_ID_KEY_NAME, "_SQL_ID_");
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setSqlIdKeyName(java.lang.String)
	 */
	@Override
	public SqlAgentProvider setSqlIdKeyName(final String sqlIdKeyName) {
		settings.put(PROPS_KEY_SQL_ID_KEY_NAME, sqlIdKeyName);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#getDefaultMapKeyCaseFormat()
	 */
	@Override
	public CaseFormat getDefaultMapKeyCaseFormat() {
		return CaseFormat.valueOf(settings.getOrDefault(PROPS_KEY_DEFAULT_MAP_KEY_CASE_FORMAT,
				CaseFormat.UPPER_SNAKE_CASE.toString()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setDefaultMapKeyCaseFormat(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public SqlAgentProvider setDefaultMapKeyCaseFormat(final CaseFormat defaultMapKeyCaseFormat) {
		settings.put(PROPS_KEY_DEFAULT_MAP_KEY_CASE_FORMAT, defaultMapKeyCaseFormat.toString());
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#getDefaultInsertsType()
	 */
	@Override
	public InsertsType getDefaultInsertsType() {
		return InsertsType.valueOf(settings.getOrDefault(PROPS_KEY_DEFAULT_INSERTS_TYPE,
				InsertsType.BATCH.toString()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setDefaultInsertsType(InsertsType)
	 */
	@Override
	public SqlAgentProvider setDefaultInsertsType(final InsertsType defaultInsertsType) {
		settings.put(PROPS_KEY_DEFAULT_INSERTS_TYPE, defaultInsertsType.toString());
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#isForceUpdateWithinTransaction()
	 */
	@Override
	public boolean isForceUpdateWithinTransaction() {
		return Boolean
				.parseBoolean(settings.getOrDefault(PROPS_KEY_FORCE_UPDATE_WITHIN_TRANSACTION,
						Boolean.FALSE.toString()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setForceUpdateWithinTransaction(boolean)
	 */
	@Override
	public SqlAgentProvider setForceUpdateWithinTransaction(final boolean forceUpdateWithinTransaction) {
		settings.put(PROPS_KEY_FORCE_UPDATE_WITHIN_TRANSACTION,
				Boolean.toString(forceUpdateWithinTransaction));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#getDefaultForUpdateWaitSeconds()
	 */
	@Override
	public int getDefaultForUpdateWaitSeconds() {
		return Integer.parseInt(settings.getOrDefault(PROPS_KEY_DEFAULT_FOR_UPDATE_WAIT_SECONDS, "10"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setDefaultForUpdateWaitSeconds(int)
	 */
	@Override
	public SqlAgentProvider setDefaultForUpdateWaitSeconds(final int defaultForUpdateWaitSeconds) {
		settings.put(PROPS_KEY_DEFAULT_FOR_UPDATE_WAIT_SECONDS, String.valueOf(defaultForUpdateWaitSeconds));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#isStrictForUpdateType()
	 */
	@Override
	public boolean isStrictForUpdateType() {
		return Boolean.parseBoolean(
				settings.getOrDefault(PROPS_KEY_STRICT_FOR_UPDATE_TYPE, Boolean.FALSE.toString()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentProvider#setStrictForUpdateType(boolean)
	 */
	@Override
	public SqlAgentProvider setStrictForUpdateType(final boolean strictForUpdateType) {
		settings.put(PROPS_KEY_STRICT_FOR_UPDATE_TYPE, Boolean.toString(strictForUpdateType));
		return this;
	}

}
