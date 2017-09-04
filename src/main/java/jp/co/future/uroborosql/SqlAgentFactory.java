package jp.co.future.uroborosql;

import java.util.List;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.store.SqlManager;

/**
 * SQL実行クラスのファクトリインターフェース。
 *
 * @author H.Sugimoto
 */
public interface SqlAgentFactory {
	/** ファクトリBean名 */
	final String FACTORY_BEAN_NAME = "sqlAagentFactory";

	/**
	 * プロパティ：終端文字を削除するかどうか.<br>
	 * デフォルトは<code>true</code>
	 */
	final String PROPS_KEY_REMOVE_TERMINATOR = "removeTerminator";

	/**
	 * プロパティ：SQL実行でエラーが発生した場合にリトライ対象とするSQLエラーコード<br>
	 * デフォルトは指定なし
	 */
	final String PROPS_KEY_SQL_RETRY_CODES = "sqlRetryCodes";

	/**
	 * プロパティ：SQL実行エラー時の最大リトライ回数デフォルト値<br>
	 * デフォルトは<code>0</code>
	 */
	final String PROPS_KEY_DEFAULT_MAX_RETRY_COUNT = "defaultMaxRetryCount";

	/**
	 * プロパティ：SQL実行リトライ時の待機時間(ms)デフォルト値<br>
	 * デフォルトは<code>0</code>
	 */
	final String PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME = "defaultSqlRetryWaitTime";

	/**
	 * プロパティ：フェッチサイズ（数値）<br>
	 * デフォルトは指定なし
	 */
	final String PROPS_KEY_FETCH_SIZE = "fetchSize";

	/**
	 * プロパティ：クエリータイムアウト（ms）（数値）<br>
	 * デフォルトは指定なし
	 */
	final String PROPS_KEY_QUERY_TIMEOUT = "queryTimeout";

	/**
	 * プロパティ：SQL_IDを置換するためのKEY文字列<br>
	 * デフォルトは "_SQL_ID_"
	 */
	final String PROPS_KEY_SQL_ID_KEY_NAME = "sqlIdKeyName";

	/**
	 * SQL実行クラス生成。
	 *
	 * @return SqlAgent
	 */
	SqlAgent createSqlAgent();

	/**
	 * SQL管理クラスを取得します。
	 *
	 * @return SQL管理クラス。
	 */
	SqlManager getSqlManager();

	/**
	 * SqlFilter管理クラスを取得します。
	 *
	 * @return SqlFilter管理クラス
	 */
	SqlFilterManager getSqlFilterManager();

	/**
	 * コネクション供給クラスを取得します。
	 *
	 * @return コネクション供給クラス
	 */
	ConnectionSupplier getConnectionSupplier();

	/**
	 * ORM処理クラスを取得します。
	 *
	 * @return ORM処理クラス
	 */
	EntityHandler<?> getEntityHandler();

	/**
	 * ORM処理クラスを設定します。
	 *
	 * @param entityHandler ORM処理クラス
	 */
	void setEntityHandler(EntityHandler<?> entityHandler);

	/**
	 * 例外発生時のログ出力を行うかどうかを取得します。
	 *
	 * @return 例外発生時のログ出力を行うかどうか。ログ出力する場合は<code>true</code>
	 */
	boolean isOutputExceptionLog();

	/**
	 * 例外発生時のログ出力を行うかどうかを設定します。
	 *
	 * @param outputExceptionLog 例外発生時のログ出力を行うかどうか。ログ出力する場合は<code>true</code>
	 */
	void setOutputExceptionLog(final boolean outputExceptionLog);

	/**
	 * 終端文字（;）を削除するかどうか を取得します
	 *
	 * @return 終端文字（;）を削除するかどうか。終端文字（;）を削除する場合<code>true</code>
	 */
	boolean isRemoveTerminator();

	/**
	 * 終端文字（;）を削除するかどうか を設定します
	 *
	 * @param removeTerminator 終端文字（;）を削除するかどうか
	 */
	void setRemoveTerminator(final boolean removeTerminator);

	/**
	 * フェッチサイズを取得します
	 *
	 * @return フェッチサイズ
	 */
	int getFetchSize();

	/**
	 * フェッチサイズを設定する
	 *
	 * @param fetchSize フェッチサイズ
	 */
	void setFetchSize(final int fetchSize);

	/**
	 * フェッチサイズを取得します
	 *
	 * @return フェッチサイズ
	 */
	int getQueryTimeout();

	/**
	 * クエリタイムアウトを設定する
	 *
	 * @param queryTimeout クエリタイムアウト
	 */
	void setQueryTimeout(final int queryTimeout);

	/**
	 * SQLをリトライ実行するSQLエラーコードのリスト を取得します
	 *
	 * @return SQLをリトライ実行するSQLエラーコードのリスト
	 */
	List<String> getSqlRetryCodeList();

	/**
	 * SQLをリトライ実行するSQLエラーコードのリスト を設定します
	 *
	 * @param sqlRetryCodeList SQLをリトライ実行するSQLエラーコードのリスト
	 */
	void setSqlRetryCodeList(final List<String> sqlRetryCodeList);

	/**
	 * 最大リトライカウントの初期値を取得します
	 *
	 * @return 最大リトライカウントの初期値
	 */
	int getDefaultMaxRetryCount();

	/**
	 * 最大リトライカウントの初期値を設定する
	 *
	 * @param defaultMaxRetryCount 最大リトライカウントの初期値
	 */
	void setDefaultMaxRetryCount(final int defaultMaxRetryCount);

	/**
	 * SQLリトライ時の待機時間（ms）の初期値を取得します
	 *
	 * @return SQLリトライ時の待機時間（ms）の初期値
	 */
	int getDefaultSqlRetryWaitTime();

	/**
	 * SQLリトライ時の待機時間（ms）の初期値を設定する
	 *
	 * @param defaultSqlRetryWaitTime SQLリトライ時の待機時間（ms）の初期値
	 */
	void setDefaultSqlRetryWaitTime(final int defaultSqlRetryWaitTime);

	/**
	 * SQL_IDを置換するためのKEY文字列を取得します
	 *
	 * @return SQL_IDを置換するためのKEY文字列
	 */
	String getSqlIdKeyName();

	/**
	 * SQL_IDを置換するためのKEY文字列を設定する
	 *
	 * @param sqlIdKeyName SQL_IDを置換するためのKEY文字列
	 */
	void setSqlIdKeyName(final String sqlIdKeyName);
}
