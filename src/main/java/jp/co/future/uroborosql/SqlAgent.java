/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.coverage.CoverageHandler;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.fluent.SqlBatch;
import jp.co.future.uroborosql.fluent.SqlEntityDelete;
import jp.co.future.uroborosql.fluent.SqlEntityQuery;
import jp.co.future.uroborosql.fluent.SqlEntityUpdate;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.fluent.SqlUpdate;
import jp.co.future.uroborosql.tx.TransactionManager;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SQL実行クラスインタフェース
 *
 * @author H.Sugimoto
 */

public interface SqlAgent extends TransactionManager {
	/**
	 * {@link SqlAgent#inserts(Stream, InsertsCondition)}の一括更新用のフレームの判定条件.<br>
	 *
	 * ex)<br>
	 * (context, count, row) -&gt; count == 1000
	 *
	 * @param <E> エンティティ型
	 */
	@FunctionalInterface
	interface InsertsCondition<E> {
		/**
		 * 一括更新用のフレームの判定
		 *
		 * @param context ExecutionContext
		 * @param count パラメーターレコード数
		 * @param entity エンティティ
		 * @return `true`の場合、一括更新を実行します
		 */
		boolean test(ExecutionContext context, int count, E entity);
	}

	/**
	 * {@link SqlAgent#updates(Stream, UpdatesCondition)}の一括更新用のフレームの判定条件.<br>
	 *
	 * ex)<br>
	 * (context, count, row) -&gt; count == 1000
	 *
	 * @param <E> エンティティ型
	 */
	@FunctionalInterface
	interface UpdatesCondition<E> {
		/**
		 * 一括更新用のフレームの判定
		 *
		 * @param context ExecutionContext
		 * @param count パラメーターレコード数
		 * @param entity エンティティ
		 * @return `true`の場合、一括更新を実行します
		 */
		boolean test(ExecutionContext context, int count, E entity);
	}

	/**
	 * SQLカバレッジを出力するかどうかのフラグ。<code>true</code>の場合はSQLカバレッジを出力する。<br>
	 * 文字列として{@link CoverageHandler}インタフェースの実装クラスが設定された場合はそのクラスを<br>
	 * 利用してカバレッジの収集を行う。
	 */
	String KEY_SQL_COVERAGE = "uroborosql.sql.coverage";

	/** BATCH-INSERT用のバッチフレームの判定条件 */
	InsertsCondition<Object> DEFAULT_BATCH_INSERTS_WHEN_CONDITION = (context, count, row) -> count == 1000;

	/** BULK-INSERT用のバッチフレームの判定条件 */
	InsertsCondition<Object> DEFAULT_BULK_INSERTS_WHEN_CONDITION = (context, count, row) -> count == 10;

	/** 一括UPDATE用のバッチフレームの判定条件 */
	UpdatesCondition<Object> DEFAULT_UPDATES_WHEN_CONDITION = (context, count, row) -> count == 1000;

	/**
	 * SqlConfigの取得.
	 *
	 * @return SqlConfig
	 */
	SqlConfig getSqlConfig();

	/**
	 * 空のExecutionContextの生成
	 *
	 * @return 生成したExecutionContext
	 */
	ExecutionContext context();

	/**
	 * ファイル指定のExecutionContextの生成
	 *
	 * @deprecated 代わりに context().setSqlName(sqlName) を利用してください
	 * @param sqlName SQLファイルのルートからの相対パス（ファイル拡張子なし）を指定
	 * @return 生成したExecutionContext
	 */
	@Deprecated(since = "1.0.0", forRemoval = true)
	ExecutionContext contextFrom(String sqlName);

	/**
	 * SQL文を指定したExecutionContextの生成
	 *
	 * @deprecated 代わりに context().setSql(sql) を利用してください
	 * @param sql SQL文の文字列
	 * @return 生成したExecutionContext
	 */
	@Deprecated(since = "1.0.0", forRemoval = true)
	ExecutionContext contextWith(String sql);

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
	CaseFormat getMapKeyCaseFormat();

	/**
	 * Queryの結果を格納するMapのキーを生成する際に使用するCaseFormatを設定する。
	 *
	 * @param mapKeyCaseFormat Queryの結果を格納するMapのキーを生成する際に使用するCaseFormat
	 */
	void setMapKeyCaseFormat(final CaseFormat mapKeyCaseFormat);

	/**
	 * {@link InsertsType}を取得する
	 *
	 * @return insertsType
	 * @see jp.co.future.uroborosql.enums.InsertsType
	 */
	InsertsType getInsertsType();

	/**
	 * {@link InsertsType}を設定する
	 *
	 * @param insertsType {@link InsertsType}
	 * @see jp.co.future.uroborosql.enums.InsertsType
	 */
	void setInsertsType(InsertsType insertsType);

	/**
	 * クエリ実行処理。
	 *
	 * @param executionContext ExecutionContext
	 * @return SQL実行結果のResultSet
	 * @throws SQLException SQL例外
	 */
	ResultSet query(ExecutionContext executionContext) throws SQLException;

	/**
	 * クエリ実行処理。<br>
	 * 結果をStreamとして返却する。
	 *
	 * @param <T> Streamの型
	 * @param executionContext ExecutionContext
	 * @param converter ResultSetの各行を変換するための変換器
	 * @return 検索結果を順次取得するStream
	 * @throws SQLException SQL例外
	 */
	<T> Stream<T> query(ExecutionContext executionContext, ResultSetConverter<T> converter) throws SQLException;

	/**
	 * クエリ実行処理。<br>
	 * 結果を{@literal List<Map<String, Object>>}に変換して返却する。
	 *
	 * @param executionContext ExecutionContext
	 * @param caseFormat Mapのキー文字列の変換書式
	 * @return 検索結果の各行のキーと値をMapに詰めたList
	 * @throws SQLException SQL例外
	 */
	List<Map<String, Object>> query(final ExecutionContext executionContext, final CaseFormat caseFormat)
			throws SQLException;

	/**
	 * DB更新処理。
	 *
	 * @param executionContext ExecutionContext
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	int update(ExecutionContext executionContext) throws SQLException;

	/**
	 * バッチ処理実行。
	 *
	 * @param executionContext ExecutionContext
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	int[] batch(ExecutionContext executionContext) throws SQLException;

	/**
	 * ストアドプロシージャ実行処理。
	 *
	 * @param executionContext ExecutionContext
	 * @return ストアドプロシージャ実行結果
	 * @throws SQLException SQL例外
	 */
	Map<String, Object> procedure(ExecutionContext executionContext) throws SQLException;

	/**
	 * Query処理の実行（Fluent API）
	 *
	 * @param sqlName 実行するSQLファイル名
	 * @return SqlQuery
	 */
	SqlQuery query(final String sqlName);

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
	 * バッチ処理の実行（Fluent API）
	 *
	 * @param sqlName 実行するSQLファイル名
	 * @return SqlBatch
	 */
	SqlBatch batch(String sqlName);

	/**
	 * バッチ処理の実行（Fluent API）
	 *
	 * @param sql 実行するSQL文
	 * @return SqlBatch
	 */
	SqlBatch batchWith(String sql);

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
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @return SQL実行結果
	 */
	<E> int insert(E entity);

	/**
	 * エンティティのINSERTを実行し、INSERTしたエンティティを返却する
	 *
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @return INSERTしたエンティティ
	 */
	<E> E insertAndReturn(E entity);

	/**
	 * エンティティのUPDATEを実行
	 *
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @return SQL実行結果
	 */
	<E> int update(E entity);

	/**
	 * エンティティのUPDATEを実行し、UPDATEしたエンティティを返却する
	 *
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @return UPDATEしたエンティティ
	 */
	<E> E updateAndReturn(E entity);

	/**
	 * エンティティのUPDATEを実行(条件指定)
	 *
	 * @param entityType エンティティタイプ
	 * @param <E> エンティティ型
	 * @return SqlEntityUpdate
	 */
	<E> SqlEntityUpdate<E> update(Class<? extends E> entityType);

	/**
	 * エンティティのMERGE（INSERTまたはUPDATE）を実行
	 *
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @return SQL実行結果
	 */
	<E> int merge(E entity);

	/**
	 * エンティティのMERGE（INSERTまたはUPDATE）を実行し、MERGEしたエンティティを返却する
	 *
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @return MERGEしたエンティティ
	 */
	<E> E mergeAndReturn(E entity);

	/**
	 * エンティティの悲観ロックを行ったうえでMERGE（INSERTまたはUPDATE）を実行
	 *
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @return SQL実行結果
	 */
	<E> int mergeWithLocking(E entity);

	/**
	 * エンティティの悲観ロックを行ったうえでMERGE（INSERTまたはUPDATE）を実行し、MERGEしたエンティティを返却する
	 *
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @return MERGEしたエンティティ
	 */
	<E> E mergeWithLockingAndReturn(E entity);

	/**
	 * エンティティのDELETEを実行
	 *
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @return SQL実行結果
	 */
	<E> int delete(E entity);

	/**
	 * エンティティのDELETEを実行し、DELETEしたエンティティを返却する
	 *
	 * @param <E> エンティティ型
	 * @param entity エンティティ
	 * @return DELETEしたエンティティ
	 */
	<E> E deleteAndReturn(E entity);

	/**
	 * エンティティのDELETEを実行(条件指定)
	 *
	 * @param entityType エンティティタイプ
	 * @param keys 削除対象PK値（複数指定可）
	 * @param <E> エンティティ型
	 * @return SQL実行結果
	 */
	<E> int delete(Class<? extends E> entityType, Object... keys);

	/**
	 * エンティティのDELETEを実行(条件指定)
	 *
	 * @param entityType エンティティタイプ
	 * @param <E> エンティティ型
	 * @return SqlEntityDelete
	 */
	<E> SqlEntityDelete<E> delete(Class<? extends E> entityType);

	/**
	 * 複数エンティティのINSERTを実行
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括INSERT用のフレームの判定条件
	 * @param insertsType INSERT処理方法
	 * @return SQL実行結果
	 */
	<E> int inserts(Class<E> entityType, Stream<E> entities, InsertsCondition<? super E> condition,
			InsertsType insertsType);

	/**
	 * 複数エンティティのINSERTを実行しINSERTしたエンティティを返却する
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括INSERT用のフレームの判定条件
	 * @param insertsType INSERT処理方法
	 * @return INSERTしたエンティティのStream
	 */
	<E> Stream<E> insertsAndReturn(Class<E> entityType, Stream<E> entities, InsertsCondition<? super E> condition,
			InsertsType insertsType);

	/**
	 * 複数エンティティのINSERTを実行
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括INSERT用のフレームの判定条件
	 * @return SQL実行結果
	 */
	<E> int inserts(Class<E> entityType, Stream<E> entities, InsertsCondition<? super E> condition);

	/**
	 * 複数エンティティのINSERTを実行しINSERTしたエンティティを返却する
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括INSERT用のフレームの判定条件
	 * @return INSERTしたエンティティのStream
	 */
	<E> Stream<E> insertsAndReturn(Class<E> entityType, Stream<E> entities, InsertsCondition<? super E> condition);

	/**
	 * 複数エンティティのINSERTを実行
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @return SQL実行結果
	 */
	<E> int inserts(Class<E> entityType, Stream<E> entities);

	/**
	 * 複数エンティティのINSERTを実行しINSERTしたエンティティを返却する.<br>
	 * 返却するエンティティはメモリ上に保持されるため、件数が多くなると{@link OutOfMemoryError}が発生する場合があります.
	 * INSERTする件数が多い場合は{@link SqlAgent#inserts(Class, Stream)}を利用してください.
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @return INSERTしたエンティティのStream
	 */
	<E> Stream<E> insertsAndReturn(Class<E> entityType, Stream<E> entities);

	/**
	 * 複数エンティティのINSERTを実行
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param insertsType INSERT処理方法
	 * @return SQL実行結果
	 */
	<E> int inserts(Class<E> entityType, Stream<E> entities, InsertsType insertsType);

	/**
	 * 複数エンティティのINSERTを実行しINSERTしたエンティティを返却する.<br>
	 * 返却するエンティティはメモリ上に保持されるため、件数が多くなると{@link OutOfMemoryError}が発生する場合があります.
	 * INSERTする件数が多い場合は{@link SqlAgent#inserts(Class, Stream, InsertsType)}を利用してください.
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param insertsType INSERT処理方法
	 * @return INSERTしたエンティティのStream
	 */
	<E> Stream<E> insertsAndReturn(Class<E> entityType, Stream<E> entities, InsertsType insertsType);

	/**
	 * 複数エンティティのINSERTを実行
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括INSERT用のフレームの判定条件
	 * @param insertsType INSERT処理方法
	 * @return SQL実行結果
	 */
	<E> int inserts(Stream<E> entities, InsertsCondition<? super E> condition, InsertsType insertsType);

	/**
	 * 複数エンティティのINSERTを実行しINSERTしたエンティティを返却する.<br>
	 * 返却するエンティティはメモリ上に保持されるため、件数が多くなると{@link OutOfMemoryError}が発生する場合があります.
	 * INSERTする件数が多い場合は{@link SqlAgent#inserts(Stream, InsertsCondition, InsertsType)}を利用してください.
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括INSERT用のフレームの判定条件
	 * @param insertsType INSERT処理方法
	 * @return INSERTしたエンティティのStream
	 */
	<E> Stream<E> insertsAndReturn(Stream<E> entities, InsertsCondition<? super E> condition, InsertsType insertsType);

	/**
	 * 複数エンティティのINSERTを実行
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括INSERT用のフレームの判定条件
	 * @return SQL実行結果
	 */
	<E> int inserts(Stream<E> entities, InsertsCondition<? super E> condition);

	/**
	 * 複数エンティティのINSERTを実行しINSERTしたエンティティを返却する.<br>
	 * 返却するエンティティはメモリ上に保持されるため、件数が多くなると{@link OutOfMemoryError}が発生する場合があります.
	 * INSERTする件数が多い場合は{@link SqlAgent#inserts(Stream, InsertsCondition)}を利用してください.
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括INSERT用のフレームの判定条件
	 * @return INSERTしたエンティティのStream
	 */
	<E> Stream<E> insertsAndReturn(Stream<E> entities, InsertsCondition<? super E> condition);

	/**
	 * 複数エンティティのINSERTを実行
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @return SQL実行結果
	 */
	<E> int inserts(Stream<E> entities);

	/**
	 * 複数エンティティのINSERTを実行しINSERTしたエンティティを返却する.<br>
	 * 返却するエンティティはメモリ上に保持されるため、件数が多くなると{@link OutOfMemoryError}が発生する場合があります.
	 * INSERTする件数が多い場合は{@link SqlAgent#inserts(Stream)}を利用してください.
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @return INSERTしたエンティティのStream
	 */
	<E> Stream<E> insertsAndReturn(Stream<E> entities);

	/**
	 * 複数エンティティのINSERTを実行
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @param insertsType INSERT処理方法
	 * @return SQL実行結果
	 */
	<E> int inserts(Stream<E> entities, InsertsType insertsType);

	/**
	 * 複数エンティティのINSERTを実行しINSERTしたエンティティを返却する.<br>
	 * 返却するエンティティはメモリ上に保持されるため、件数が多くなると{@link OutOfMemoryError}が発生する場合があります.
	 * INSERTする件数が多い場合は{@link SqlAgent#inserts(Stream, InsertsType)}を利用してください.
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @param insertsType INSERT処理方法
	 * @return INSERTしたエンティティのStream
	 */
	<E> Stream<E> insertsAndReturn(Stream<E> entities, InsertsType insertsType);

	/**
	 * 複数エンティティのUPDATEを実行
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括更新用のフレームの判定条件
	 * @return SQL実行結果
	 */
	<E> int updates(Class<E> entityType, Stream<E> entities, UpdatesCondition<? super E> condition);

	/**
	 * 複数エンティティのUPDATEを実行しUPDATEしたエンティティを返却する.<br>
	 * 返却するエンティティはメモリ上に保持されるため、件数が多くなると{@link OutOfMemoryError}が発生する場合があります.
	 * UPDATEする件数が多い場合は{@link SqlAgent#updates(Class, Stream, UpdatesCondition)}を利用してください.
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括更新用のフレームの判定条件
	 * @return UPDATEしたエンティティのStream
	 */
	<E> Stream<E> updatesAndReturn(Class<E> entityType, Stream<E> entities, UpdatesCondition<? super E> condition);

	/**
	 * 複数エンティティのUPDATEを実行
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @return SQL実行結果
	 */
	<E> int updates(Class<E> entityType, Stream<E> entities);

	/**
	 * 複数エンティティのUPDATEを実行しUPDATEしたエンティティを返却する.<br>
	 * 返却するエンティティはメモリ上に保持されるため、件数が多くなると{@link OutOfMemoryError}が発生する場合があります.
	 * UPDATEする件数が多い場合は{@link SqlAgent#updates(Class, Stream)}を利用してください.
	 *
	 * @param <E> エンティティの型
	 * @param entityType エンティティの型
	 * @param entities エンティティ
	 * @return UPDATEしたエンティティのStream
	 */
	<E> Stream<E> updatesAndReturn(Class<E> entityType, Stream<E> entities);

	/**
	 * 複数エンティティのUPDATEを実行
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括更新用のフレームの判定条件
	 * @return SQL実行結果
	 */
	<E> int updates(Stream<E> entities, UpdatesCondition<? super E> condition);

	/**
	 * 複数エンティティのUPDATEを実行しUPDATEしたエンティティを返却する.<br>
	 * 返却するエンティティはメモリ上に保持されるため、件数が多くなると{@link OutOfMemoryError}が発生する場合があります.
	 * UPDATEする件数が多い場合は{@link SqlAgent#updates(Stream, UpdatesCondition)}を利用してください.
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @param condition 一括更新用のフレームの判定条件
	 * @return UPDATEしたエンティティのStream
	 */
	<E> Stream<E> updatesAndReturn(Stream<E> entities, UpdatesCondition<? super E> condition);

	/**
	 * 複数エンティティのUPDATEを実行
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @return SQL実行結果
	 */
	<E> int updates(Stream<E> entities);

	/**
	 * 複数エンティティのUPDATEを実行しUPDATEしたエンティティを返却する.<br>
	 * 返却するエンティティはメモリ上に保持されるため、件数が多くなると{@link OutOfMemoryError}が発生する場合があります.
	 * UPDATEする件数が多い場合は{@link SqlAgent#updates(Stream)}を利用してください.
	 *
	 * @param <E> エンティティの型
	 * @param entities エンティティ
	 * @return UPDATEしたエンティティのStream
	 */
	<E> Stream<E> updatesAndReturn(Stream<E> entities);

	/**
	 * エンティティで指定されたテーブルのtruncateを行う.<br>
	 * 引き続きSQL操作を行うため、戻り値としてSqlAgentを返却する.
	 *
	 * @param <E> エンティティの型
	 * @param entityType truncateするテーブルのEntity型
	 * @return SqlAgent
	 */
	<E> SqlAgent truncate(Class<? extends E> entityType);

}