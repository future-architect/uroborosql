package jp.co.future.uroborosql.mapping;

import java.sql.SQLException;
import java.util.Map;
import java.util.stream.Stream;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.connection.ConnectionManager;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.EntityResultSetConverter;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapper;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;

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
	TableMetadata getMetadata(ConnectionManager connectionManager, Class<? extends ENTITY> entityType) throws SQLException;

	/**
	 * パラメータからSELECT SQLコンテキストを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entityType エンティティタイプ
	 * @param params パラメータ
	 * @return INSERT SQLコンテキスト
	 */
	SqlContext createSelectContext(SqlAgent agent, TableMetadata metadata, Class<? extends ENTITY> entityType, Map<String, ?> params);

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
	default <E> Stream<E> doSelect(final SqlAgent agent, final SqlContext context, final Class<? extends E> entityType) throws SQLException {
		return agent.query(context, new EntityResultSetConverter<>(entityType, new PropertyMapperManager()));
	}

	/**
	 * EntityからINSERT SQLコンテキストを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entity エンティティ
	 * @return INSERT SQLコンテキスト
	 */
	SqlContext createInsertContext(SqlAgent agent, TableMetadata metadata, ENTITY entity);

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
	 * EntityからUPDATE SQLコンテキストを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entity エンティティ
	 * @return UPDATE SQLコンテキスト
	 */
	SqlContext createUpdateContext(SqlAgent agent, TableMetadata metadata, ENTITY entity);

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
	 * EntityからDELETE SQLコンテキストを生成します。
	 *
	 * @param agent SqlAgent
	 * @param metadata エンティティメタ情報
	 * @param entity エンティティ
	 * @return DELETE SQLコンテキスト
	 */
	SqlContext createDeleteContext(SqlAgent agent, TableMetadata metadata, ENTITY entity);

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
	 */
	void addPropertyMapper(PropertyMapper<?> propertyMapper);

	/**
	 * プロパティ変換クラス{@link PropertyMapper}をremove
	 *
	 * @param propertyMapper {@link PropertyMapper}
	 */
	void removePropertyMapper(PropertyMapper<?> propertyMapper);
}
