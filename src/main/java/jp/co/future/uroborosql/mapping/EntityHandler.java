/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import java.sql.SQLException;
import java.util.stream.Stream;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.SqlConfigAware;
import jp.co.future.uroborosql.connection.ConnectionManager;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapper;

/**
 * ORM処理インタフェース
 *
 * @param <ENTITY> エンティティクラス
 * @author ota
 */
public interface EntityHandler<ENTITY> extends SqlConfigAware {
	/**
	 * 初期化処理
	 */
	void initialize();

	/**
	 * エンティティ型を返す
	 *
	 * @return エンティティ型
	 */
	Class<ENTITY> getEntityType();

	/**
	 * エンティティ型からエンティティメタ情報を取得します。
	 *
	 * @param connectionManager コネクションマネージャー
	 * @param entityType エンティティタイプ
	 * @return エンティティメタ情報
	 * @throws SQLException SQL例外
	 */
	TableMetadata getMetadata(ConnectionManager connectionManager, Class<? extends ENTITY> entityType)
			throws SQLException;

	/**
	 * エンティティタイプからSELECT ExecutionContextを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @param addCondition 条件を追加するかどうか。追加する場合<code>true</code>
	 * @return SELECT ExecutionContext
	 */
	ExecutionContext createSelectContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType,
			boolean addCondition);

	/**
	 * EntityからINSERT ExecutionContextを生成します。
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 * @param entityType エンティティタイプ
	 * @param <E> エンティティ型
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	<E> Stream<E> doSelect(final SqlAgent agent, final ExecutionContext context, final Class<? extends E> entityType)
			throws SQLException;

	/**
	 * エンティティタイプからINSERT ExecutionContextを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @return INSERT ExecutionContext
	 */
	ExecutionContext createInsertContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType);

	/**
	 * ExecutionContextのパラメーターににエンティティの値をセットします。
	 *
	 * @param context ExecutionContext
	 * @param entity エンティティ
	 */
	void setInsertParams(final ExecutionContext context, final ENTITY entity);

	/**
	 * INSERTを実行します。
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 * @param entity エンティティ
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	default int doInsert(final SqlAgent agent, final ExecutionContext context, final ENTITY entity) throws SQLException {
		return agent.update(context);
	}

	/**
	 * エンティティタイプからUPDATE ExecutionContextを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @param addCondition 条件を追加するかどうか。追加する場合<code>true</code>
	 * @return UPDATE ExecutionContext
	 */
	ExecutionContext createUpdateContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType,
			boolean addCondition);

	/**
	 * ExecutionContextのパラメーターににエンティティの値をセットします。
	 *
	 * @param context ExecutionContext
	 * @param entity エンティティ
	 */
	void setUpdateParams(final ExecutionContext context, final ENTITY entity);

	/**
	 * UPDATEを実行します。
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 * @param entity エンティティ
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	default int doUpdate(final SqlAgent agent, final ExecutionContext context, final ENTITY entity) throws SQLException {
		return agent.update(context);
	}

	/**
	 * エンティティタイプからDELETE ExecutionContextを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @param addCondition 条件を追加するかどうか。追加する場合<code>true</code>
	 * @return DELETE ExecutionContext
	 */
	ExecutionContext createDeleteContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType,
			boolean addCondition);

	/**
	 * ExecutionContextのパラメーターににエンティティの値をセットします。
	 *
	 * @param context ExecutionContext
	 * @param entity エンティティ
	 */
	void setDeleteParams(final ExecutionContext context, final ENTITY entity);

	/**
	 * DELETEを実行します。
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 * @param entity エンティティ
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	default int doDelete(final SqlAgent agent, final ExecutionContext context, final ENTITY entity) throws SQLException {
		return agent.update(context);
	}

	/**
	 * エンティティタイプからバッチ用INSERT ExecutionContextを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @return INSERT ExecutionContext
	 */
	ExecutionContext createBatchInsertContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType);

	/**
	 * BATCH INSERTを実行します。
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	default int[] doBatchInsert(final SqlAgent agent, final ExecutionContext context) throws SQLException {
		return agent.batch(context);
	}

	/**
	 * エンティティタイプからBULK INSERT ExecutionContextを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @return INSERT ExecutionContext
	 */
	ExecutionContext createBulkInsertContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType);

	/**
	 * ExecutionContextのパラメーターににエンティティの値をセットします。
	 *
	 * @param context ExecutionContext
	 * @param entity エンティティ
	 * @param entityIndex エンティティのインデックス
	 */
	void setBulkInsertParams(final ExecutionContext context, final ENTITY entity, int entityIndex);

	/**
	 * BULK INSERT ExecutionContextにSQLを設定します。
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @param numberOfRecords レコード行数
	 * @return INSERT ExecutionContext
	 */
	ExecutionContext setupSqlBulkInsertContext(SqlAgent agent, ExecutionContext context, TableMetadata metadata,
			Class<? extends ENTITY> entityType, int numberOfRecords);

	/**
	 * BULK INSERTを実行します。
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	default int doBulkInsert(final SqlAgent agent, final ExecutionContext context)
			throws SQLException {
		return agent.update(context);
	}

	/**
	 * エンティティタイプからバッチ用UPDATE ExecutionContextを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @return UPDATE ExecutionContext
	 */
	ExecutionContext createBatchUpdateContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType);

	/**
	 * BATCH UPDATEを実行します。
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	default int[] doBatchUpdate(final SqlAgent agent, final ExecutionContext context) throws SQLException {
		return agent.batch(context);
	}

	/**
	 * プロパティ変換クラス{@link PropertyMapper}を追加
	 *
	 * @param propertyMapper {@link PropertyMapper}
	 * @return EntityHandler
	 */
	EntityHandler<ENTITY> addPropertyMapper(PropertyMapper<?> propertyMapper);

	/**
	 * プロパティ変換クラス{@link PropertyMapper}をremove
	 *
	 * @param propertyMapper {@link PropertyMapper}
	 * @return EntityHandler
	 */
	EntityHandler<ENTITY> removePropertyMapper(PropertyMapper<?> propertyMapper);

	/**
	 * 空文字とNULLを同じに扱うかどうかを取得する
	 *
	 * @return 空文字とNULLを同じに扱う場合<code>true</code>
	 */
	boolean isEmptyStringEqualsNull();

	/**
	 * 空文字とNULLを同じに扱うかどうかを設定する
	 *
	 * @param emptyStringEqualsNull 空文字とNULLを同じに扱う場合に<code>true</code>を指定
	 * @return EntityHandler
	 */
	EntityHandler<ENTITY> setEmptyStringEqualsNull(boolean emptyStringEqualsNull);
}
