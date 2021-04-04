/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.util.List;

import jp.co.future.uroborosql.config.SqlConfigAware;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.exception.UroborosqlTransactionException;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SQL実行クラスのプロバイダインターフェース。
 *
 * @author H.Sugimoto
 */
public interface SqlAgentProvider extends SqlConfigAware {
	/** プロバイダBean名 */
	String FACTORY_BEAN_NAME = "sqlAgentProvider";

	/**
	 * プロパティ:SQL実行でエラーが発生した場合にリトライ対象とするSQLエラーコード<br>
	 * デフォルトは指定なし
	 */
	String PROPS_KEY_SQL_RETRY_CODES = "sqlRetryCodes";

	/**
	 * プロパティ:SQL実行エラー時の最大リトライ回数デフォルト値<br>
	 * デフォルトは<code>0</code>
	 */
	String PROPS_KEY_DEFAULT_MAX_RETRY_COUNT = "defaultMaxRetryCount";

	/**
	 * プロパティ:SQL実行リトライ時の待機時間(ms)デフォルト値<br>
	 * デフォルトは<code>0</code>
	 */
	String PROPS_KEY_DEFAULT_SQL_RETRY_WAIT_TIME = "defaultSqlRetryWaitTime";

	/**
	 * プロパティ:フェッチサイズ（数値）<br>
	 * デフォルトは指定なし
	 */
	String PROPS_KEY_FETCH_SIZE = "fetchSize";

	/**
	 * プロパティ:クエリータイムアウト（ms）（数値）<br>
	 * デフォルトは指定なし
	 */
	String PROPS_KEY_QUERY_TIMEOUT = "queryTimeout";

	/**
	 * プロパティ:SQL_IDを置換するためのKEY文字列<br>
	 * デフォルトは "_SQL_ID_"
	 */
	String PROPS_KEY_SQL_ID_KEY_NAME = "sqlIdKeyName";

	/**
	 * プロパティ:Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat<br>
	 * デフォルトは "UPPER_SNAKE_CASE"
	 */
	String PROPS_KEY_DEFAULT_MAP_KEY_CASE_FORMAT = "defaultMapKeyCaseFormat";

	/**
	 * プロパティ:デフォルトの{@link InsertsType}<br>
	 * デフォルトは "BULK"
	 */
	String PROPS_KEY_DEFAULT_INSERTS_TYPE = "defaultInsertsType";

	/**
	 * プロパティ:トランザクション内での更新を強制するかどうか<br>
	 * デフォルトは false
	 */
	String PROPS_KEY_FORCE_UPDATE_WITHIN_TRANSACTION = "forceUpdateWithinTransaction";

	/**
	 * プロパティ:明示的な行ロック時の待機時間(s)デフォルト値<br>
	 * デフォルトは<code>10</code>
	 */
	String PROPS_KEY_DEFAULT_FOR_UPDATE_WAIT_SECONDS = "defaultForUpdateWaitSeconds";

	/**
	 * プロパティ:ForUpdateTypeの指定を厳格に扱うかどうか<br>
	 * デフォルトは<code>false</code>
	 */
	String PROPS_KEY_STRICT_FOR_UPDATE_TYPE = "strictForUpdateType";

	/**
	 * SqlAgentの生成.
	 *
	 * @return SqlAgent
	 */
	SqlAgent agent();

	/**
	 * SqlAgentの生成.
	 *
	 * @param connectionContext DB接続情報
	 *
	 * @return 生成したSqlAgent.
	 */
	SqlAgent agent(ConnectionContext connectionContext);

	/**
	 * 例外発生時のログ出力を行うかどうかを取得します。
	 *
	 * @return 例外発生時のログ出力を行うかどうか。ログ出力する場合は<code>true</code>
	 */
	boolean isOutputExceptionLog();

	/**
	 * 例外発生時のログ出力を行うかどうかを設定する.
	 *
	 * @param outputExceptionLog 例外発生時のログ出力を行うかどうか。ログ出力する場合は<code>true</code>
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setOutputExceptionLog(final boolean outputExceptionLog);

	/**
	 * フェッチサイズを取得する.
	 *
	 * @return フェッチサイズ
	 */
	int getFetchSize();

	/**
	 * フェッチサイズを設定する.
	 *
	 * @param fetchSize フェッチサイズ
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setFetchSize(final int fetchSize);

	/**
	 * フェッチサイズを取得する.
	 *
	 * @return フェッチサイズ
	 */
	int getQueryTimeout();

	/**
	 * クエリタイムアウトを設定する.
	 *
	 * @param queryTimeout クエリタイムアウト
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setQueryTimeout(final int queryTimeout);

	/**
	 * SQLをリトライ実行するSQLエラーコードのリスト を取得する.
	 *
	 * @return SQLをリトライ実行するSQLエラーコードのリスト
	 */
	List<String> getSqlRetryCodeList();

	/**
	 * SQLをリトライ実行するSQLエラーコードのリスト を設定する.
	 *
	 * @param sqlRetryCodeList SQLをリトライ実行するSQLエラーコードのリスト
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setSqlRetryCodeList(final List<String> sqlRetryCodeList);

	/**
	 * 最大リトライカウントの初期値を取得する.
	 *
	 * @return 最大リトライカウントの初期値
	 */
	int getDefaultMaxRetryCount();

	/**
	 * 最大リトライカウントの初期値を設定する.
	 *
	 * @param defaultMaxRetryCount 最大リトライカウントの初期値
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setDefaultMaxRetryCount(final int defaultMaxRetryCount);

	/**
	 * SQLリトライ時の待機時間（ms）の初期値を取得する.
	 *
	 * @return SQLリトライ時の待機時間（ms）の初期値
	 */
	int getDefaultSqlRetryWaitTime();

	/**
	 * SQLリトライ時の待機時間（ms）の初期値を設定する.
	 *
	 * @param defaultSqlRetryWaitTime SQLリトライ時の待機時間（ms）の初期値
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setDefaultSqlRetryWaitTime(final int defaultSqlRetryWaitTime);

	/**
	 * SQL_IDを置換するためのKEY文字列を取得する.
	 *
	 * @return SQL_IDを置換するためのKEY文字列
	 */
	String getSqlIdKeyName();

	/**
	 * SQL_IDを置換するためのKEY文字列を設定する.
	 *
	 * @param sqlIdKeyName SQL_IDを置換するためのKEY文字列
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setSqlIdKeyName(final String sqlIdKeyName);

	/**
	 * Queryの結果を格納するMapのキーを生成する際に使用するCaseFormatを取得する.
	 *
	 * @return Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat
	 */
	CaseFormat getDefaultMapKeyCaseFormat();

	/**
	 * Queryの結果を格納するMapのキーを生成する際に使用するCaseFormatを設定する.
	 *
	 * @param defaultMapKeyCaseFormat Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setDefaultMapKeyCaseFormat(CaseFormat defaultMapKeyCaseFormat);

	/**
	 * デフォルトの{@link InsertsType}を取得する.
	 *
	 * @return insertsType
	 * @see jp.co.future.uroborosql.enums.InsertsType
	 */
	InsertsType getDefaultInsertsType();

	/**
	 * デフォルトの{@link InsertsType}を設定する.
	 *
	 * @param defaultInsertsType デフォルトの{@link InsertsType}
	 * @return SqlAgentProvider
	 * @see jp.co.future.uroborosql.enums.InsertsType
	 */
	SqlAgentProvider setDefaultInsertsType(InsertsType defaultInsertsType);

	/**
	 * トランザクション内での更新を強制するかどうかを取得する.
	 *
	 * @return トランザクション内でのみ更新可能とする場合<code>true</code>
	 */
	boolean isForceUpdateWithinTransaction();

	/**
	 * トランザクション内での更新を強制するかどうかを設定する.<br>
	 * <code>true</code>を指定するとトランザクションを開始していない状態で SELECT文以外のSQLを発行すると {@link UroborosqlTransactionException}をスローする
	 *
	 * @param forceUpdateWithinTransaction トランザクション内でのみ更新可能とするかどうか。
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setForceUpdateWithinTransaction(boolean forceUpdateWithinTransaction);

	/**
	 * 明示的な行ロック時の待機時間(s)デフォルト値を取得する.
	 *
	 * @return 明示的な行ロック時の待機時間(s)デフォルト値
	 */
	int getDefaultForUpdateWaitSeconds();

	/**
	 * 明示的な行ロック時の待機時間(s)デフォルト値を設定する.
	 *
	 * @param defaultForUpdateWaitSeconds 明示的な行ロック時の待機時間(s)デフォルト値
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setDefaultForUpdateWaitSeconds(final int defaultForUpdateWaitSeconds);

	/**
	 * ForUpdateTypeの指定を厳格に扱うかどうかを取得する.
	 *
	 * @return ForUpdateTypeの指定を厳格に扱うかどうか
	 */
	boolean isStrictForUpdateType();

	/**
	 * ForUpdateTypeの指定を厳格に扱うかどうかを設定する.
	 *
	 * @param strictForUpdateType ForUpdateTypeの指定を厳格に扱うかどうか
	 * @return SqlAgentProvider
	 */
	SqlAgentProvider setStrictForUpdateType(boolean strictForUpdateType);

}
