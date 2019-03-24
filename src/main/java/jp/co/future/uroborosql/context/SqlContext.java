/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.context;

import java.math.BigDecimal;
import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLType;
import java.util.Map;

import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.fluent.SqlFluent;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * SQLコンテキストインタフェース
 *
 * @author H.Sugimoto
 *
 */
public interface SqlContext extends TransformContext, SqlFluent<SqlContext> {

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
	 * @return 自身のSqlContext
	 */
	SqlContext setSql(String sql);

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
	 * @return 自身のSqlContext
	 */
	SqlContext setSqlName(String sqlName);

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
	 * @return 自身のSqlContext
	 */
	SqlContext setSqlId(String sqlId);

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
	 * @return 自身のSqlContext
	 */
	SqlContext setMaxRetryCount(int maxRetryCount);

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
	 * @return 自身のSqlContext
	 */
	SqlContext setRetryWaitTime(int retryWaitTime);

	/**
	 * ステートメントにパラメータをバインドする
	 *
	 * @param preparedStatement バインドを行うステートメント
	 * @throws SQLException SQL例外
	 */
	void bindParams(PreparedStatement preparedStatement) throws SQLException;

	/**
	 * ステートメントにバッチ用のパラメータをバインドする
	 *
	 * @param preparedStatement バインドを行うステートメント
	 * @throws SQLException SQL例外
	 */
	void bindBatchParams(PreparedStatement preparedStatement) throws SQLException;

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
	 * @return 自身のSqlContext
	 */
	SqlContext addBatch();

	/**
	 * これまでに追加されたパラメータ群をバッチパラメータから削除する
	 *
	 * @return 自身のSqlContext
	 */
	SqlContext clearBatch();

	/**
	 * addBatchされた回数を取得する
	 * @return バッチ回数
	 */
	int batchCount();

	/**
	 * 自動パラメータバインド関数(query用)の受け入れ
	 */
	void acceptQueryAutoParameterBinder();

	/**
	 * 自動パラメータバインド関数(update/batch/proc用)の受け入れ
	 */
	void acceptUpdateAutoParameterBinder();

	/**
	 * 列型の定義追加<br>
	 *
	 * @param column カラム番号
	 * @param type {@link java.sql.Types}で表されるSQLの型
	 */
	void addDefineColumnType(int column, int type);

	/**
	 * 列型の定義追加<br>
	 *
	 * @param column カラム番号
	 * @param type {@link java.sql.SQLType}で表されるSQLの型
	 */
	default void addDefineColumnType(final int column, final SQLType type) {
		addDefineColumnType(column, type.getVendorTypeNumber());
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
	 */
	void setResultSetType(int resultSetType);

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
	 */
	void setResultSetConcurrency(int resultSetConcurrency);

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
	 */
	void setSqlKind(SqlKind sqlKind);

	/**
	 *  DB接続の別名（エイリアス）を設定する
	 *
	 * @param dbAlias DB接続の別名（エイリアス）
	 */
	void setDBAlias(String dbAlias);

	/**
	 * DB接続の別名（エイリアス）
	 *
	 * @return DB接続の別名（エイリアス）
	 */
	String getDbAlias();

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
	 */
	void setGeneratedKeyColumns(String[] generatedKeyColumns);

	/**
	 * 自動採番するキーカラム値の配列を取得する
	 *
	 * @return 自動採番するキーカラム値の配列
	 */
	BigDecimal[] getGeneratedKeyValues();

	/**
	 * 自動採番するキーカラム値の配列を設定する
	 *
	 * @param generatedKeyValues 自動採番するキーカラム値の配列
	 */
	void setGeneratedKeyValues(BigDecimal[] generatedKeyValues);
}