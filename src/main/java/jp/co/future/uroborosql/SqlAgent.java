package jp.co.future.uroborosql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.fluent.SqlUpdate;
import jp.co.future.uroborosql.tx.TransactionManager;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SQL実行クラスインタフェース
 *
 * @author H.Sugimoto
 *
 */

public interface SqlAgent extends AutoCloseable, TransactionManager {

	/**
	 * クエリ実行処理。<br />
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return SQL実行結果のResultSet
	 * @throws SQLException SQL例外
	 */
	ResultSet query(SqlContext sqlContext) throws SQLException;

	/**
	 * クエリ実行処理。<br />
	 * 結果をStreamとして返却する。
	 *
	 * @param sqlContext SqlContext
	 * @param converter ResultSetの各行を変換するための変換器
	 * @return 検索結果を順次取得するStream
	 * @throws SQLException SQL例外
	 */
	<T> Stream<T> query(SqlContext sqlContext, ResultSetConverter<T> converter) throws SQLException;

	/**
	 * クエリ実行処理。<br />
	 * 結果をList<Map<String, Object>>に変換して返却する。
	 *
	 * @param sqlContext SqlContext
	 * @param caseFormat Mapのキー文字列の変換書式
	 * @return 検索結果の各行のキーと値をMapに詰めたList
	 * @throws SQLException SQL例外
	 */
	List<Map<String, Object>> query(final SqlContext sqlContext, final CaseFormat caseFormat) throws SQLException;

	/**
	 * DB更新処理。<br />
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	int update(SqlContext sqlContext) throws SQLException;

	/**
	 * バッチ処理実行。
	 *
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	int[] batch(SqlContext sqlContext) throws SQLException;

	/**
	 * ストアドプロシージャ実行処理。<br />
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return ストアドプロシージャ実行結果
	 * @throws SQLException SQL例外
	 */
	Map<String, Object> procedure(SqlContext sqlContext) throws SQLException;

	/**
	 * クローズ処理
	 *
	 * @see java.lang.AutoCloseable#close()
	 *
	 * @throws SQLException SQL例外
	 */
	@Override
	void close() throws SQLException;

	/**
	 * フェッチサイズ取得。
	 *
	 * @return フェッチサイズ
	 */
	int getFetchSize();

	/**
	 * フェッチサイズ設定。
	 *
	 * @param fetchSize フェッチサイズ
	 */
	void setFetchSize(int fetchSize);

	/**
	 * クエリータイムアウト制限値取得。
	 *
	 * @return クエリータイムアウト制限値
	 */
	int getQueryTimeout();

	/**
	 * クエリータイムアウト制限値設定。
	 *
	 * @param queryTimeout クエリータイムアウト制限値
	 */
	void setQueryTimeout(int queryTimeout);

	/**
	 * 終端文字（;）を削除するかどうか を取得します
	 *
	 * @return 終端文字（;）を削除する場合<code>true</code>
	 */
	boolean isRemoveTerminator();

	/**
	 * 終端文字（;）を削除するかどうか を設定します
	 *
	 * @param removeTerminator 終端文字（;）を削除する場合<code>true</code>
	 */
	void setRemoveTerminator(boolean removeTerminator);

	/**
	 * トランザクションのコミット用API
	 *
	 * 実装はオプション。APIを提供しない場合は{@link UnsupportedOperationException}をスローすること
	 *
	 * @throws SQLException SQL例外. トランザクションのコミットに失敗した場合
	 * @throws UnsupportedOperationException この操作を提供しない場合
	 */
	@Override
	void commit() throws SQLException;

	/**
	 * トランザクションのロールバック用API
	 *
	 * 実装はオプション。APIを提供しない場合は{@link UnsupportedOperationException}をスローすること
	 *
	 * @throws SQLException SQL例外. トランザクションのロールバックに失敗した場合
	 * @throws UnsupportedOperationException. この操作を提供しない場合
	 */
	@Override
	void rollback() throws SQLException;

	/**
	 * SQL設定管理クラスの取得
	 *
	 * @return SQL設定管理クラスインスタンス
	 */
	SqlConfig getSqlConfig();

	/**
	 * SQL設定管理クラスの設定
	 *
	 * @param config SQL設定管理クラスインスタンス
	 */
	void setSqlConfig(SqlConfig config);

	/**
	 * 空のSqlContextの生成
	 *
	 * @return 生成したSqlContext
	 */
	SqlContext context();

	/**
	 * ファイル指定のSqlContextの生成
	 *
	 * @param sqlName SQLファイルのルートからの相対パス（ファイル拡張子なし）を指定
	 * @return 生成したSqlContext
	 */
	SqlContext contextFrom(String sqlName);

	/**
	 * SQL文を指定したSqlContextの生成
	 *
	 * @param sql SQL文の文字列
	 * @return 生成したSqlContext
	 */
	SqlContext contextWith(String sql);

	/**
	 * Query処理の実行（Fluent API）
	 *
	 * @param sqlName 実行するSQLファイル名
	 * @return SqlQuery
	 */
	SqlQuery query(String sqlName);

	/**
	 * Query処理の実行（Fluent API）
	 *
	 * @param sql 実行するSQL文
	 * @return SqlQuery
	 */
	SqlQuery queryWith(String sql);

	/**
	 * 更新処理の実行（Fluent API）
	 *
	 * @param sqlName 実行するSQLファイル名
	 * @return SqlUpdate
	 */
	SqlUpdate update(String sqlName);

	/**
	 * 更新処理の実行（Fluent API）
	 *
	 * @param sql 実行するSQL文
	 * @return SqlUpdate
	 */
	SqlUpdate updateWith(String sql);

}