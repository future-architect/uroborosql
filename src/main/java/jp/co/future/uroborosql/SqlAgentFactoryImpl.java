package jp.co.future.uroborosql;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.DefaultEntityHandler;
import jp.co.future.uroborosql.mapping.EntityHandler;
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

	/** ORM処理クラス */
	private EntityHandler<?> entityHandler;

	/** デフォルト値を保持するプロパティ */
	private final Map<String, String> defaultProps = new HashMap<>();

	/**
	 * コンストラクタ。
	 */
	public SqlAgentFactoryImpl() {
		this(null, null, null, null);
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
		this(connectionSupplier, sqlManager, sqlFilterManager, null);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param connectionSupplier コネクション供給クラス
	 * @param sqlManager SQL管理クラス
	 * @param sqlFilterManager SQLフィルタ管理クラス
	 * @param entityHandler ORM処理クラス
	 *
	 */
	public SqlAgentFactoryImpl(final ConnectionSupplier connectionSupplier, final SqlManager sqlManager,
			final SqlFilterManager sqlFilterManager, final EntityHandler<?> entityHandler) {
		this.connectionSupplier = connectionSupplier;
		this.sqlManager = sqlManager;
		this.sqlFilterManager = sqlFilterManager;
		this.entityHandler = entityHandler != null ? entityHandler : new DefaultEntityHandler();
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
		return new SqlAgentImpl(getConnectionSupplier(), getSqlManager(), getSqlFilterManager(), getEntityHandler(), getDefaultProps());
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
	 * @see jp.co.future.uroborosql.SqlAgentFactory#getEntityHandler()
	 */
	@Override
	public EntityHandler<?> getEntityHandler() {
		return entityHandler;
	}

	/**
	 * ORM処理クラスを設定します。
	 *
	 * @param entityHandler SQLフィルタ管理クラス
	 */
	public void setEntityHandler(final EntityHandler<?> entityHandler) {
		this.entityHandler = entityHandler;
	}

	/**
	 * 例外発生時のログ出力を行うかどうかを取得します。
	 *
	 * @return 例外発生時のログ出力を行うかどうか。ログ出力する場合は<code>true</code>
	 */
	public boolean isOutputExceptionLog() {
		return Boolean.parseBoolean(getDefaultProps().get(PROPS_KEY_OUTPUT_EXCEPTION_LOG));
	}

	/**
	 * 例外発生時のログ出力を行うかどうかを設定します。
	 *
	 * @param outputExceptionLog 例外発生時のログ出力を行うかどうか。ログ出力する場合は<code>true</code>
	 */
	public void setOutputExceptionLog(final boolean outputExceptionLog) {
		getDefaultProps().put(PROPS_KEY_OUTPUT_EXCEPTION_LOG, Boolean.toString(outputExceptionLog));
	}

	/**
	 * 終端文字（;）を削除するかどうか を取得します
	 *
	 * @return 終端文字（;）を削除するかどうか。終端文字（;）を削除する場合<code>true</code>
	 */
	public boolean isRemoveTerminator() {
		return Boolean.parseBoolean(getDefaultProps().get(PROPS_KEY_REMOVE_TERMINATOR));
	}

	/**
	 * 終端文字（;）を削除するかどうか を設定します
	 *
	 * @param removeTerminator 終端文字（;）を削除するかどうか
	 */
	public void setRemoveTerminator(final boolean removeTerminator) {
		getDefaultProps().put(PROPS_KEY_REMOVE_TERMINATOR, Boolean.toString(removeTerminator));
	}

	/**
	 * フェッチサイズを取得します
	 *
	 * @return フェッチサイズ
	 */
	public int getFetchSize() {
		if (getDefaultProps().containsKey(PROPS_KEY_FETCH_SIZE)) {
			return Integer.parseInt(getDefaultProps().get(PROPS_KEY_FETCH_SIZE));
		} else {
			return -1;
		}
	}

	/**
	 * フェッチサイズを設定する
	 *
	 * @param fetchSize フェッチサイズ
	 */
	public void setFetchSize(final int fetchSize) {
		getDefaultProps().put(PROPS_KEY_FETCH_SIZE, String.valueOf(fetchSize));
	}

	/**
	 * フェッチサイズを取得します
	 *
	 * @return フェッチサイズ
	 */
	public int getQueryTimeout() {
		if (getDefaultProps().containsKey(PROPS_KEY_QUERY_TIMEOUT)) {
			return Integer.parseInt(getDefaultProps().get(PROPS_KEY_QUERY_TIMEOUT));
		} else {
			return -1;
		}
	}

	/**
	 * クエリタイムアウトを設定する
	 *
	 * @param queryTimeout クエリタイムアウト
	 */
	public void setQueryTimeout(final int queryTimeout) {
		getDefaultProps().put(PROPS_KEY_QUERY_TIMEOUT, String.valueOf(queryTimeout));
	}

	/**
	 * SQLをリトライ実行するSQLエラーコードのリスト を取得します
	 *
	 * @return SQLをリトライ実行するSQLエラーコードのリスト
	 */
	public List<String> getSqlRetryCodeList() {
		String codes = getDefaultProps().get(PROPS_KEY_SQL_RETRY_CODES);
		if (codes == null) {
			return Collections.emptyList();
		} else {
			return Arrays.asList(codes.split(","));
		}
	}

	/**
	 * SQLをリトライ実行するSQLエラーコードのリスト を設定します
	 *
	 * @param sqlRetryCodeList SQLをリトライ実行するSQLエラーコードのリスト
	 */
	public void setSqlRetryCodeList(final List<String> sqlRetryCodeList) {
		if (sqlRetryCodeList != null && !sqlRetryCodeList.isEmpty()) {
			getDefaultProps().put(PROPS_KEY_SQL_RETRY_CODES, String.join(",", sqlRetryCodeList));
		}
	}

	/**
	 * 最大リトライカウントの初期値を取得します
	 *
	 * @return 最大リトライカウントの初期値
	 */
	public int getDefaultMaxRetryCount() {
		if (getDefaultProps().containsKey(PROPS_KEY_DEFAULT_MAX_RETRY_COUNT)) {
			return Integer.parseInt(getDefaultProps().get(PROPS_KEY_DEFAULT_MAX_RETRY_COUNT));
		} else {
			return 0;
		}
	}

	/**
	 * 最大リトライカウントの初期値を設定する
	 *
	 * @param defaultMaxRetryCount 最大リトライカウントの初期値
	 */
	public void setDefaultMaxRetryCount(final int defaultMaxRetryCount) {
		getDefaultProps().put(PROPS_KEY_DEFAULT_MAX_RETRY_COUNT, String.valueOf(defaultMaxRetryCount));
	}

	/**
	 * SQLリトライ時の待機時間（ms）の初期値を取得します
	 *
	 * @return SQLリトライ時の待機時間（ms）の初期値
	 */
	public int getDefaultSqlRetryWaitTime() {
		if (getDefaultProps().containsKey(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME)) {
			return Integer.parseInt(getDefaultProps().get(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME));
		} else {
			return 0;
		}
	}

	/**
	 * SQLリトライ時の待機時間（ms）の初期値を設定する
	 *
	 * @param defaultSqlRetryWaitTime SQLリトライ時の待機時間（ms）の初期値
	 */
	public void setDefaultSqlRetryWaitTime(final int defaultSqlRetryWaitTime) {
		getDefaultProps().put(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME, String.valueOf(defaultSqlRetryWaitTime));
	}

	/**
	 * SQL_IDを置換するためのKEY文字列を取得します
	 *
	 * @return SQL_IDを置換するためのKEY文字列
	 */
	public String getSqlIdKeyName() {
		if (getDefaultProps().containsKey(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME)) {
			return getDefaultProps().get(PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME);
		} else {
			return "_SQL_ID_";
		}
	}

	/**
	 * SQL_IDを置換するためのKEY文字列を設定する
	 *
	 * @param sqlIdKeyName SQL_IDを置換するためのKEY文字列
	 */
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
