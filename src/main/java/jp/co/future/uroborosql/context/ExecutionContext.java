/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.context;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLType;
import java.time.Clock;
import java.util.Map;
import java.util.function.Function;

import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.fluent.ProcedureFluent;
import jp.co.future.uroborosql.fluent.SqlFluent;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * ExecutionContextインタフェース
 *
 * @author H.Sugimoto
 *
 */
public interface ExecutionContext
		extends TransformContext, SqlFluent<ExecutionContext>, ProcedureFluent<ExecutionContext> {

	/**
	 * 変換前SQL取得
	 *
	 * @return 変換前SQL
	 */
	String getSql();

	/**
	 * 変換前SQL設定
	 *
	 * @param sql 変換前SQL
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setSql(String sql);

	/**
	 * SQL名取得
	 *
	 * @return SQLファイルのルートからの相対パス（ファイル拡張子なし）
	 */
	String getSqlName();

	/**
	 * SQL名設定
	 *
	 * @param sqlName SQLファイルのルートからの相対パス（ファイル拡張子なし）を指定
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setSqlName(String sqlName);

	/**
	 * SQL文を識別するための文字列を取得
	 *
	 * @return SQL識別子
	 */
	String getSqlId();

	/**
	 * SQL文を識別するための文字列を設定
	 *
	 * @param sqlId SQL識別子
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setSqlId(String sqlId);

	/**
	 * SQLを実行するスキーマを取得
	 *
	 * @return スキーマ
	 */
	String getSchema();

	/**
	 * SQLを実行するスキーマを設定
	 *
	 * @param schema スキーマ
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setSchema(String schema);

	/**
	 * 最大リトライ回数 を取得する
	 *
	 * @return 最大リトライ回数
	 */
	int getMaxRetryCount();

	/**
	 * 最大リトライ回数 を設定する
	 *
	 * @param maxRetryCount 最大リトライ回数
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setMaxRetryCount(int maxRetryCount);

	/**
	 * リトライ待機時間（ms） を取得する
	 *
	 * @return リトライ待機時間（ms）
	 */
	int getRetryWaitTime();

	/**
	 * リトライ待機時間（ms） を設定する
	 *
	 * @param retryWaitTime リトライ待機時間（ms）
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setRetryWaitTime(int retryWaitTime);

	/**
	 * ステートメントにパラメータをバインドする
	 *
	 * @param preparedStatement バインドを行うステートメント
	 * @return 自身のExecutionContext
	 * @throws SQLException SQL例外
	 */
	ExecutionContext bindParams(PreparedStatement preparedStatement) throws SQLException;

	/**
	 * ステートメントにバッチ用のパラメータをバインドする
	 *
	 * @param preparedStatement バインドを行うステートメント
	 * @return 自身のExecutionContext
	 * @throws SQLException SQL例外
	 */
	ExecutionContext bindBatchParams(PreparedStatement preparedStatement) throws SQLException;

	/**
	 * 出力パラメータの取得
	 *
	 * @param callableStatement コーラブルステートメント
	 * @return 出力パラメータのMap
	 * @throws SQLException SQL例外
	 */
	Map<String, Object> getOutParams(CallableStatement callableStatement) throws SQLException;

	/**
	 * これまでに追加されたパラメータ群をバッチパラメータに格納する
	 *
	 * @return 自身のExecutionContext
	 */
	ExecutionContext addBatch();

	/**
	 * これまでに追加されたパラメータ群をバッチパラメータから削除する
	 *
	 * @return 自身のExecutionContext
	 */
	ExecutionContext clearBatch();

	/**
	 * addBatchされた回数を取得する
	 * @return バッチ回数
	 */
	int batchCount();

	/**
	 * 列型の定義追加<br>
	 *
	 * @param column カラム番号
	 * @param type {@link java.sql.Types}で表されるSQLの型
	 * @return 自身のExecutionContext
	 */
	ExecutionContext addDefineColumnType(int column, int type);

	/**
	 * 列型の定義追加<br>
	 *
	 * @param column カラム番号
	 * @param type {@link java.sql.SQLType}で表されるSQLの型
	 * @return 自身のExecutionContext
	 */
	default ExecutionContext addDefineColumnType(final int column, final SQLType type) {
		addDefineColumnType(column, type.getVendorTypeNumber());
		return this;
	}

	/**
	 * 列型の定義情報を取得する
	 * @return 列の型定義情報
	 */
	Map<Integer, Integer> getDefineColumnTypes();

	/**
	 * 結果セットの型の設定<BR>
	 * {@link ResultSet#TYPE_FORWARD_ONLY}、{@link ResultSet#TYPE_SCROLL_INSENSITIVE}、
	 * {@link ResultSet#TYPE_SCROLL_SENSITIVE} のうちいづれか 1 つ
	 *
	 * @param resultSetType 結果セットの型
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setResultSetType(int resultSetType);

	/**
	 * 結果セットの型の取得
	 *
	 * @return 結果セットの型
	 */
	int getResultSetType();

	/**
	 * 並行処理の種類の設定<BR>
	 * {@link ResultSet#CONCUR_READ_ONLY} または {@link ResultSet#CONCUR_UPDATABLE}
	 *
	 * @param resultSetConcurrency 並行処理の種類
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setResultSetConcurrency(int resultSetConcurrency);

	/**
	 * 並行処理の種類の取得
	 *
	 * @return 並行処理の種類
	 */
	int getResultSetConcurrency();

	/**
	 * 実行するSQLの種別を取得する
	 *
	 * @return SQL種別
	 */
	SqlKind getSqlKind();

	/**
	 * 実行するSQLの種別を設定する
	 *
	 * @param sqlKind SQL種別
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setSqlKind(SqlKind sqlKind);

	/**
	 * コンテキストが保持する属性を取得する
	 * @return コンテキスト属性情報
	 */
	Map<String, Object> contextAttrs();

	/**
	 * バインドパラメータの文字列表現を返す
	 *
	 * @return バインドパラメータの文字列表現
	 */
	String formatParams();

	/**
	 * 自動採番するキーカラム名の配列を取得する
	 *
	 * @return 自動採番するキーカラム名の配列
	 */
	String[] getGeneratedKeyColumns();

	/**
	 * 自動採番するキーカラム名の配列を設定する
	 *
	 * @param generatedKeyColumns 自動採番するキーカラム名の配列
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setGeneratedKeyColumns(String[] generatedKeyColumns);

	/**
	 * 自動採番するキーカラム値の配列を取得する
	 *
	 * @return 自動採番するキーカラム値の配列
	 */
	Object[] getGeneratedKeyValues();

	/**
	 * 自動採番するキーカラム名の配列に値が設定されているか
	 *
	 * @return 値が設定されている場合<code>true</code>
	 */
	boolean hasGeneratedKeyColumns();

	/**
	 * 自動採番するキーカラム値の配列を設定する
	 *
	 * @param generatedKeyValues 自動採番するキーカラム値の配列
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setGeneratedKeyValues(Object[] generatedKeyValues);

	/**
	 * 更新処理実行時に通常の更新SQL発行の代わりに移譲する処理を取得する.<br>
	 * デフォルト実装は<code>null</code> を返却する. 必要に応じて子クラスでオーバーライドすること.
	 *
	 * @return 通常の更新SQL発行の代わりに行う疑似動作. <code>null</code> が返る場合は通常の更新処理を行う.
	 */
	default Function<ExecutionContext, Integer> getUpdateDelegate() {
		return null;
	}

	/**
	 * 更新処理実行時に通常の更新SQL発行の代わりに移譲する処理を設定する.
	 *
	 * @param updateDelegate 通常の更新SQL発行の代わりに移譲する処理. <code>null</code> を設定した場合は通常の更新処理を行う.
	 * @return 自身のExecutionContext
	 */
	ExecutionContext setUpdateDelegate(Function<ExecutionContext, Integer> updateDelegate);

	/**
	 * SqlConfigが保持するClockを取得する.
	 *
	 * @return SqlConfigが保持するClock
	 */
	Clock getClock();
}