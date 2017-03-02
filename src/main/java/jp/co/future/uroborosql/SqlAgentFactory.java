package jp.co.future.uroborosql;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.filter.SqlFilterManager;
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
	 * プロパティ：終端文字を削除するかどうか.
	 * デフォルトは<code>true</code>
	 */
	final String PROPS_KEY_REMOVE_TERMINATOR = "removeTerminator";

	/**
	 * プロパティ：SQL実行でエラーが発生した場合にリトライ対象とするSQLエラーコード
	 * デフォルトは指定なし
	 */
	final String PROPS_KEY_SQL_RETRY_CODES = "sqlRetryCodes";

	/**
	 * プロパティ：SQL実行エラー時の最大リトライ回数デフォルト値
	 * デフォルトは<code>0</code>
	 */
	final String PROPS_KEY_DEFAULT_MAX_RETRY_COUNT = "defaultMaxRetryCount";

	/**
	 * プロパティ：SQL実行リトライ時の待機時間(ms)デフォルト値
	 * デフォルトは<code>0</code>
	 */
	final String PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME = "defaultSqlRetryWaitTime";

	/**
	 * プロパティ：フェッチサイズ（数値）
	 * デフォルトは指定なし
	 */
	final String PROPS_KEY_FETCH_SIZE = "fetchSize";

	/**
	 * プロパティ：クエリータイムアウト（ms）（数値）
	 * デフォルトは指定なし
	 */
	final String PROPS_KEY_QUERY_TIMEOUT = "queryTimeout";

	/**
	 * プロパティ：SQL_IDを置換するためのKEY文字列
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
}
