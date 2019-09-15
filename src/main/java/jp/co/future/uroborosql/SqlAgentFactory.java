/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.util.List;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.exception.UroborosqlTransactionException;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.store.SqlManager;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SQL実行クラスのファクトリインターフェース。
 *
 * @author H.Sugimoto
 */
public interface SqlAgentFactory {
	/** ファクトリBean名 */
	String FACTORY_BEAN_NAME = "sqlAagentFactory";

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
	 * SqlConfigの設定[
	 *
	 * @param sqlConfig SqlConfig
	 */
	void setSqlConfig(final SqlConfig sqlConfig);

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
	 * @return SqlAgentFactory
	 */
	SqlAgentFactory setOutputExceptionLog(final boolean outputExceptionLog);

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
	 * @return SqlAgentFactory
	 */
	SqlAgentFactory setFetchSize(final int fetchSize);

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
	 * @return SqlAgentFactory
	 */
	SqlAgentFactory setQueryTimeout(final int queryTimeout);

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
	 * @return SqlAgentFactory
	 */
	SqlAgentFactory setSqlRetryCodeList(final List<String> sqlRetryCodeList);

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
	 * @return SqlAgentFactory
	 */
	SqlAgentFactory setDefaultMaxRetryCount(final int defaultMaxRetryCount);

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
	 * @return SqlAgentFactory
	 */
	SqlAgentFactory setDefaultSqlRetryWaitTime(final int defaultSqlRetryWaitTime);

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
	 * @return SqlAgentFactory
	 */
	SqlAgentFactory setSqlIdKeyName(final String sqlIdKeyName);

	/**
	 * Queryの結果を格納するMapのキーを生成する際に使用するCaseFormatを取得する
	 *
	 * @return Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat
	 */
	CaseFormat getDefaultMapKeyCaseFormat();

	/**
	 * Queryの結果を格納するMapのキーを生成する際に使用するCaseFormatを設定する
	 *
	 * @param defaultMapKeyCaseFormat Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat
	 * @return SqlAgentFactory
	 */
	SqlAgentFactory setDefaultMapKeyCaseFormat(CaseFormat defaultMapKeyCaseFormat);

	/**
	 * デフォルトの{@link InsertsType}を取得する
	 *
	 * @return insertsType
	 * @see jp.co.future.uroborosql.enums.InsertsType
	 */
	InsertsType getDefaultInsertsType();

	/**
	 * デフォルトの{@link InsertsType}を設定する
	 *
	 * @param defaultInsertsType デフォルトの{@link InsertsType}
	 * @return SqlAgentFactory
	 * @see jp.co.future.uroborosql.enums.InsertsType
	 */
	SqlAgentFactory setDefaultInsertsType(InsertsType defaultInsertsType);

	/**
	 * トランザクション内での更新を強制するかどうかを取得する
	 * @return トランザクション内でのみ更新可能とする場合<true>
	 */
	boolean isForceUpdateWithinTransaction();

	/**
	 * トランザクション内での更新を強制するかどうかを設定する<br>
	 * <code>true</code>を指定するとトランザクションを開始していない状態で SELECT文以外のSQLを発行すると {@link UroborosqlTransactionException}をスローする
	 *
	 * @param forceUpdateWithinTransaction トランザクション内でのみ更新可能とするかどうか。
	 * @return SqlAgentFactory
	 */
	SqlAgentFactory setForceUpdateWithinTransaction(boolean forceUpdateWithinTransaction);
}
