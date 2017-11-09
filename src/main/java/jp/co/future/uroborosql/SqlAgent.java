package jp.co.future.uroborosql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.coverage.CoverageHandler;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.fluent.SqlEntityQuery;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.fluent.SqlUpdate;
import jp.co.future.uroborosql.tx.TransactionManager;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SQL実行クラスインタフェース
 *
 * @author H.Sugimoto
 */

public interface SqlAgent extends AutoCloseable, TransactionManager {
	/**
	 * SQLカバレッジを出力するかどうかのフラグ。<code>true</code>の場合はSQLカバレッジを出力する。<br>
	 * 文字列として{@link CoverageHandler}インタフェースの実装クラスが設定された場合はそのクラスを<br>
	 * 利用してカバレッジの収集を行う。
	 */
	final String KEY_SQL_COVERAGE = "uroborosql.sql.coverage";

	/**
	 * クエリ実行処理。
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return SQL実行結果のResultSet
	 * @throws SQLException SQL例外
	 */
	ResultSet query(SqlContext sqlContext) throws SQLException;

	/**
	 * クエリ実行処理。<br>
	 * 結果をStreamとして返却する。
	 *
	 * @param <T> Streamの型
	 * @param sqlContext SqlContext
	 * @param converter ResultSetの各行を変換するための変換器
	 * @return 検索結果を順次取得するStream
	 * @throws SQLException SQL例外
	 */
	<T> Stream<T> query(SqlContext sqlContext, ResultSetConverter<T> converter) throws SQLException;

	/**
	 * クエリ実行処理。<br>
	 * 結果を{@literal List<Map<String, Object>>}に変換して返却する。
	 *
	 * @param sqlContext SqlContext
	 * @param caseFormat Mapのキー文字列の変換書式
	 * @return 検索結果の各行のキーと値をMapに詰めたList
	 * @throws SQLException SQL例外
	 */
	List<Map<String, Object>> query(final SqlContext sqlContext, final CaseFormat caseFormat) throws SQLException;

	/**
	 * DB更新処理。
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	int update(SqlContext sqlContext) throws SQLException;

	/**
	 * バッチ処理実行。
	 *
	 * @param sqlContext SQLコンテキスト
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	int[] batch(SqlContext sqlContext) throws SQLException;

	/**
	 * ストアドプロシージャ実行処理。
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
	 */
	@Override
	void close();

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
	 * Queryの結果を格納するMapのキーを生成する際に使用するCaseFormatを取得する
	 *
	 * @return Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat
	 */
	CaseFormat getDefaultMapKeyCaseFormat();

	/**
	 * Queryの結果を格納するMapのキーを生成する際に使用するCaseFormatを設定する。
	 *
	 * @param defaultMapKeyCaseFormat Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat
	 */
	void setDefaultMapKeyCaseFormat(final CaseFormat defaultMapKeyCaseFormat);

	/**
	 * トランザクションのコミット用API<br>
	 * <br>
	 * 実装はオプション。APIを提供しない場合は{@link UnsupportedOperationException}をスローすること
	 *
	 * @throws UnsupportedOperationException この操作を提供しない場合
	 */
	@Override
	void commit();

	/**
	 * トランザクションのロールバック用API<br>
	 * <br>
	 * 実装はオプション。APIを提供しない場合は{@link UnsupportedOperationException}をスローすること
	 *
	 * @throws UnsupportedOperationException この操作を提供しない場合
	 */
	@Override
	void rollback();

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

	/**
	 * Procedureの実行（Fluent API）
	 *
	 * @param sqlName 実行するSQLファイル名
	 * @return Procedure
	 */
	Procedure proc(String sqlName);

	/**
	 * Procedureの実行（Fluent API）
	 *
	 * @param sql 実行するSQL文
	 * @return Procedure
	 */
	Procedure procWith(String sql);

	/**
	 * キーを指定したエンティティの1件取得を実行
	 *
	 * @param entityType エンティティタイプ
	 * @param keys キー
	 * @param <E> エンティティ型
	 * @return SQL実行結果
	 */
	<E> Optional<E> find(Class<? extends E> entityType, Object... keys);

	/**
	 * エンティティを指定して Query処理の実行
	 *
	 * @param entityType エンティティタイプ
	 * @param <E> エンティティ型
	 * @return SqlEntityQuery
	 */
	<E> SqlEntityQuery<E> query(Class<? extends E> entityType);

	/**
	 * エンティティのINSERTを実行
	 *
	 * @param entity エンティティ
	 * @return SQL実行結果
	 */
	int insert(Object entity);

	/**
	 * エンティティのUPDATEを実行
	 *
	 * @param entity エンティティ
	 * @return SQL実行結果
	 */
	int update(Object entity);

	/**
	 * エンティティのDELETEを実行
	 *
	 * @param entity エンティティ
	 * @return SQL実行結果
	 */
	int delete(Object entity);
}