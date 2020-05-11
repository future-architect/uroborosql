/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.store.SqlManager;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * Sql実行クラスのファクトリクラス。
 *
 * @author H.Sugimoto
 */
public class SqlAgentFactoryImpl implements SqlAgentFactory {
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
	public SqlAgentFactoryImpl() {
		this(null);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param sqlConfig SQL設定管理クラス
	 */
	public SqlAgentFactoryImpl(final SqlConfig sqlConfig) {
		this.sqlConfig = sqlConfig;
		settings.put(PROPS_KEY_OUTPUT_EXCEPTION_LOG, Boolean.TRUE.toString());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @deprecated Instead, use the agent() method.
	 * @see jp.co.future.uroborosql.SqlAgentFactory#createSqlAgent()
	 */
	@Deprecated
	@Override
	public SqlAgent createSqlAgent() {
		return this.agent();
	}

	@Override
	public SqlAgent agent() {
		return this.agent(null);
	}

	@Override
	public SqlAgent agent(final Map<String, String> props) {
		if (sqlConfig == null) {
			throw new UroborosqlRuntimeException();
		}
		return new SqlAgentImpl(sqlConfig, settings, props);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getConnectionSupplier()
	 */
	@Override
	public ConnectionSupplier getConnectionSupplier() {
		return sqlConfig == null ? null : sqlConfig.getConnectionSupplier();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getSqlManager()
	 */
	@Override
	public SqlManager getSqlManager() {
		return sqlConfig == null ? null : sqlConfig.getSqlManager();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getSqlFilterManager()
	 */
	@Override
	public SqlFilterManager getSqlFilterManager() {
		return sqlConfig == null ? null : sqlConfig.getSqlFilterManager();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getEntityHandler()
	 */
	@Override
	public EntityHandler<?> getEntityHandler() {
		return sqlConfig == null ? null : sqlConfig.getEntityHandler();
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
	 * @see jp.co.future.uroborosql.SqlAgentFactory#isOutputExceptionLog()
	 */
	@Override
	public boolean isOutputExceptionLog() {
		return Boolean.parseBoolean(settings.get(PROPS_KEY_OUTPUT_EXCEPTION_LOG));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setOutputExceptionLog(boolean)
	 */
	@Override
	public SqlAgentFactory setOutputExceptionLog(final boolean outputExceptionLog) {
		settings.put(PROPS_KEY_OUTPUT_EXCEPTION_LOG, Boolean.toString(outputExceptionLog));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getFetchSize()
	 */
	@Override
	public int getFetchSize() {
		return Integer.parseInt(settings.getOrDefault(PROPS_KEY_FETCH_SIZE, "-1"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setFetchSize(int)
	 */
	@Override
	public SqlAgentFactory setFetchSize(final int fetchSize) {
		settings.put(PROPS_KEY_FETCH_SIZE, String.valueOf(fetchSize));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getQueryTimeout()
	 */
	@Override
	public int getQueryTimeout() {
		return Integer.parseInt(settings.getOrDefault(PROPS_KEY_QUERY_TIMEOUT, "-1"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setQueryTimeout(int)
	 */
	@Override
	public SqlAgentFactory setQueryTimeout(final int queryTimeout) {
		settings.put(PROPS_KEY_QUERY_TIMEOUT, String.valueOf(queryTimeout));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getSqlRetryCodeList()
	 */
	@Override
	public List<String> getSqlRetryCodeList() {
		String codes = settings.get(PROPS_KEY_SQL_RETRY_CODES);
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
	public SqlAgentFactory setSqlRetryCodeList(final List<String> sqlRetryCodeList) {
		if (sqlRetryCodeList != null && !sqlRetryCodeList.isEmpty()) {
			settings.put(PROPS_KEY_SQL_RETRY_CODES, String.join(",", sqlRetryCodeList));
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getDefaultMaxRetryCount()
	 */
	@Override
	public int getDefaultMaxRetryCount() {
		return Integer.parseInt(settings.getOrDefault(PROPS_KEY_DEFAULT_MAX_RETRY_COUNT, "0"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setDefaultMaxRetryCount(int)
	 */
	@Override
	public SqlAgentFactory setDefaultMaxRetryCount(final int defaultMaxRetryCount) {
		settings.put(PROPS_KEY_DEFAULT_MAX_RETRY_COUNT, String.valueOf(defaultMaxRetryCount));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getDefaultSqlRetryWaitTime()
	 */
	@Override
	public int getDefaultSqlRetryWaitTime() {
		return Integer.parseInt(settings.getOrDefault(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME, "0"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setDefaultSqlRetryWaitTime(int)
	 */
	@Override
	public SqlAgentFactory setDefaultSqlRetryWaitTime(final int defaultSqlRetryWaitTime) {
		settings.put(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME, String.valueOf(defaultSqlRetryWaitTime));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getSqlIdKeyName()
	 */
	@Override
	public String getSqlIdKeyName() {
		return settings.getOrDefault(PROPS_KEY_SQL_ID_KEY_NAME, "_SQL_ID_");
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setSqlIdKeyName(java.lang.String)
	 */
	@Override
	public SqlAgentFactory setSqlIdKeyName(final String sqlIdKeyName) {
		settings.put(PROPS_KEY_SQL_ID_KEY_NAME, sqlIdKeyName);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getDefaultMapKeyCaseFormat()
	 */
	@Override
	public CaseFormat getDefaultMapKeyCaseFormat() {
		return CaseFormat.valueOf(settings.getOrDefault(PROPS_KEY_DEFAULT_MAP_KEY_CASE_FORMAT,
				CaseFormat.UPPER_SNAKE_CASE.toString()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setDefaultMapKeyCaseFormat(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public SqlAgentFactory setDefaultMapKeyCaseFormat(final CaseFormat defaultMapKeyCaseFormat) {
		settings.put(PROPS_KEY_DEFAULT_MAP_KEY_CASE_FORMAT, defaultMapKeyCaseFormat.toString());
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getDefaultInsertsType()
	 */
	@Override
	public InsertsType getDefaultInsertsType() {
		return InsertsType.valueOf(settings.getOrDefault(PROPS_KEY_DEFAULT_INSERTS_TYPE,
				InsertsType.BULK.toString()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setDefaultInsertsType(InsertsType)
	 */
	@Override
	public SqlAgentFactory setDefaultInsertsType(final InsertsType defaultInsertsType) {
		settings.put(PROPS_KEY_DEFAULT_INSERTS_TYPE, defaultInsertsType.toString());
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#isForceUpdateWithinTransaction()
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
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setForceUpdateWithinTransaction(boolean)
	 */
	@Override
	public SqlAgentFactory setForceUpdateWithinTransaction(final boolean forceUpdateWithinTransaction) {
		settings.put(PROPS_KEY_FORCE_UPDATE_WITHIN_TRANSACTION,
				Boolean.toString(forceUpdateWithinTransaction));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getDefaultForUpdateWaitSeconds()
	 */
	@Override
	public int getDefaultForUpdateWaitSeconds() {
		return Integer.parseInt(settings.getOrDefault(PROPS_KEY_DEFAULT_FOR_UPDATE_WAIT_SECONDS, "10"));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setDefaultForUpdateWaitSeconds(int)
	 */
	@Override
	public SqlAgentFactory setDefaultForUpdateWaitSeconds(final int defaultForUpdateWaitSeconds) {
		settings.put(PROPS_KEY_DEFAULT_FOR_UPDATE_WAIT_SECONDS, String.valueOf(defaultForUpdateWaitSeconds));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#isStrictForUpdateType()
	 */
	@Override
	public boolean isStrictForUpdateType() {
		return Boolean.parseBoolean(
				settings.getOrDefault(PROPS_KEY_STRICT_FOR_UPDATE_TYPE, Boolean.FALSE.toString()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.SqlAgentFactory#setStrictForUpdateType(boolean)
	 */
	@Override
	public SqlAgentFactory setStrictForUpdateType(final boolean strictForUpdateType) {
		settings.put(PROPS_KEY_STRICT_FOR_UPDATE_TYPE, Boolean.toString(strictForUpdateType));
		return this;
	}

}
