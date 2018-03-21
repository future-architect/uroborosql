package jp.co.future.uroborosql.mapping;

import java.sql.SQLException;
import java.util.stream.Stream;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.connection.ConnectionManager;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapper;

/**
 * ORM処理インタフェース
 *
 * @param <ENTITY> エンティティクラス
 * @author ota
 */
public interface EntityHandler<ENTITY> {
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
	 * エンティティタイプからSELECT SQLコンテキストを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @return SELECT SQLコンテキスト
	 */
	SqlContext createSelectContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType);

	/**
	 * EntityからINSERT SQLコンテキストを生成します。
	 *
	 * @param agent SqlAgent
	 * @param context SQLコンテキスト
	 * @param entityType エンティティタイプ
	 * @param <E> エンティティ型
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	<E> Stream<E> doSelect(final SqlAgent agent, final SqlContext context, final Class<? extends E> entityType)
			throws SQLException;

	/**
	 * エンティティタイプからINSERT SQLコンテキストを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @return INSERT SQLコンテキスト
	 */
	SqlContext createInsertContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType);

	/**
	 * SqlContextのパラメーターににエンティティの値をセットします。
	 *
	 * @param context SQLコンテキスト
	 * @param entity エンティティ
	 */
	void setInsertParams(final SqlContext context, final ENTITY entity);

	/**
	 * INSERTを実行します。
	 *
	 * @param agent SqlAgent
	 * @param context SQLコンテキスト
	 * @param entity エンティティ
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	default int doInsert(final SqlAgent agent, final SqlContext context, final ENTITY entity) throws SQLException {
		return agent.update(context);
	}

	/**
	 * エンティティタイプからUPDATE SQLコンテキストを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @return UPDATE SQLコンテキスト
	 */
	SqlContext createUpdateContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType);

	/**
	 * SqlContextのパラメーターににエンティティの値をセットします。
	 *
	 * @param context SQLコンテキスト
	 * @param entity エンティティ
	 */
	void setUpdateParams(final SqlContext context, final ENTITY entity);

	/**
	 * UPDATEを実行します。
	 *
	 * @param agent SqlAgent
	 * @param context SQLコンテキスト
	 * @param entity エンティティ
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	default int doUpdate(final SqlAgent agent, final SqlContext context, final ENTITY entity) throws SQLException {
		return agent.update(context);
	}

	/**
	 * エンティティタイプからDELETE SQLコンテキストを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @return DELETE SQLコンテキスト
	 */
	SqlContext createDeleteContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType);

	/**
	 * SqlContextのパラメーターににエンティティの値をセットします。
	 *
	 * @param context SQLコンテキスト
	 * @param entity エンティティ
	 */
	void setDeleteParams(final SqlContext context, final ENTITY entity);

	/**
	 * DELETEを実行します。
	 *
	 * @param agent SqlAgent
	 * @param context SQLコンテキスト
	 * @param entity エンティティ
	 * @return SQL実行結果
	 * @throws SQLException SQL例外
	 */
	default int doDelete(final SqlAgent agent, final SqlContext context, final ENTITY entity) throws SQLException {
		return agent.update(context);
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
