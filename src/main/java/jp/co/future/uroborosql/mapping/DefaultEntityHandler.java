package jp.co.future.uroborosql.mapping;

import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.connection.ConnectionManager;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.EntityResultSetConverter;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapper;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;

/**
 * デフォルトORM処理クラス
 *
 * @author ota
 */
public class DefaultEntityHandler implements EntityHandler<Object> {

	private static Map<Class<?>, TableMetadata> CONTEXTS = new ConcurrentHashMap<>();
	private final PropertyMapperManager propertyMapperManager = new PropertyMapperManager();

	@Override
	public SqlContext createSelectContext(final SqlAgent agent, final TableMetadata metadata, final Class<? extends Object> entityType) {
		return agent.contextWith(buildSelectSQL(metadata, entityType));
	}

	@Override
	public <E> Stream<E> doSelect(final SqlAgent agent, final SqlContext context, final Class<? extends E> entityType) throws SQLException {
		return agent.query(context, new EntityResultSetConverter<>(entityType, new PropertyMapperManager(propertyMapperManager)));
	}

	@Override
	public SqlContext createInsertContext(final SqlAgent agent, final TableMetadata metadata, final Class<? extends Object> entityType) {
		return agent.contextWith(buildInsertSQL(metadata, entityType));
	}

	@Override
	public SqlContext createUpdateContext(final SqlAgent agent, final TableMetadata metadata, final Class<? extends Object> entityType) {
		return agent.contextWith(buildUpdateSQL(metadata, entityType));
	}

	@Override
	public SqlContext createDeleteContext(final SqlAgent agent, final TableMetadata metadata, final Class<? extends Object> entityType) {
		return agent.contextWith(buildDeleteSQL(metadata, entityType));
	}

	@Override
	public void setInsertParams(final SqlContext context, final Object entity) {
		setFields(context, entity);
	}

	@Override
	public void setUpdateParams(final SqlContext context, final Object entity) {
		setFields(context, entity);
	}

	@Override
	public void setDeleteParams(final SqlContext context, final Object entity) {
		setFields(context, entity);
	}

	@Override
	public Class<Object> getEntityType() {
		return Object.class;
	}

	@Override
	public TableMetadata getMetadata(final ConnectionManager connectionManager, final Class<?> entityType) throws SQLException {
		TableMetadata context = CONTEXTS.get(entityType);
		if (context == null) {
			context = createMetadata(connectionManager, entityType);
			CONTEXTS.put(entityType, context);
		}
		return context;
	}

	@Override
	public void addPropertyMapper(final PropertyMapper<?> propertyMapper) {
		propertyMapperManager.addMapper(propertyMapper);
	}

	@Override
	public void removePropertyMapper(final PropertyMapper<?> propertyMapper) {
		propertyMapperManager.removeMapper(propertyMapper);
	}

	/**
	 * エンティティ型から、EntityMetadataの生成
	 *
	 * @param connectionManager コネクションマネージャー
	 * @param type エンティティ型
	 * @return TableMetadata
	 * @throws SQLException SQL例外
	 */
	protected TableMetadata createMetadata(final ConnectionManager connectionManager, final Class<? extends Object> type)
			throws SQLException {
		Table table = geTable(type);
		return TableMetadata.createTableEntityMetadata(connectionManager, table);
	}

	/**
	 * エンティティ型からテーブル情報の取得
	 *
	 * @param type エンティティ型
	 * @return テーブル情報
	 */
	protected Table geTable(final Class<? extends Object> type) {
		return MappingUtils.geTable(type);
	}

	/**
	 * SELECT SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @return SELECT SQL
	 */
	protected String buildSelectSQL(final TableMetadata metadata, final Class<? extends Object> type) {
		List<? extends TableMetadata.Column> columns = metadata.getColumns();
		int columnSize = columns.size();

		StringBuilder sql = new StringBuilder("SELECT /* mapping @ ")
				.append(type.getSimpleName())
				.append(" */\n");

		for (int i = 0; i < columnSize; i++) {
			TableMetadata.Column col = columns.get(i);
			if (i > 0) {
				sql.append(", ");
			}
			sql.append(col.getColumnName()).append("\n");
		}

		sql.append("FROM ").append(metadata.getTableIdentifier()).append("\n");
		sql.append("WHERE 1 = 1\n");

		for (TableMetadata.Column col : columns) {
			String camelColName = col.getCamelColumnName();
			sql.append("/*IF \"").append(camelColName).append("\" in getParameterNames() */\n");// フィールドがセットされていない場合はカラム自体を削る
			sql.append("AND ").append(col.getColumnName()).append(" = ").append("/*").append(camelColName).append("*/''\n");
			sql.append("/*END*/\n");
		}

		List<? extends TableMetadata.Column> keys = metadata.getKeyColumns();

		for (int i = 0; i < keys.size(); i++) {
			TableMetadata.Column col = keys.get(i);
			if (i == 0) {
				sql.append("ORDER BY \n");
			} else {
				sql.append(", ");
			}
			sql.append(col.getColumnName()).append("\n");
		}

		return sql.toString();
	}

	/**
	 * INSERT SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @return INSERT SQL
	 */
	protected String buildInsertSQL(final TableMetadata metadata, final Class<? extends Object> type) {
		List<? extends TableMetadata.Column> columns = metadata.getColumns();

		int columnSize = columns.size();

		StringBuilder sql = new StringBuilder("INSERT /* mapping @ ")
				.append(type.getSimpleName())
				.append(" */ INTO ")
				.append(metadata.getTableIdentifier())
				.append("(\n");

		for (int i = 0; i < columnSize; i++) {
			TableMetadata.Column col = columns.get(i);
			String camelColName = col.getCamelColumnName();
			sql.append("/*IF \"").append(camelColName).append("\" in getParameterNames() */\n");// フィールドがセットされていない場合はカラム自体を削る
			if (i > 0) {
				appendIfCommma(sql, columns, i);
			}
			sql.append(col.getColumnName()).append("\n");
			sql.append("/*END*/\n");
		}

		sql.append(") VALUES (\n");

		for (int i = 0; i < columnSize; i++) {
			TableMetadata.Column col = columns.get(i);
			String camelColName = col.getCamelColumnName();
			sql.append("/*IF \"").append(camelColName).append("\" in getParameterNames() */\n");// フィールドがセットされていない場合はカラム自体を削る
			if (i > 0) {
				appendIfCommma(sql, columns, i);
			}
			sql.append("/*").append(camelColName).append("*/''\n");
			sql.append("/*END*/\n");
		}

		sql.append(")");
		return sql.toString();
	}

	/**
	 * UPDATE SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @return UPDATE SQL
	 */
	protected String buildUpdateSQL(final TableMetadata metadata, final Class<? extends Object> type) {
		List<? extends TableMetadata.Column> columns = metadata.getColumns();

		int columnSize = columns.size();

		StringBuilder sql = new StringBuilder("UPDATE /* mapping @ ")
				.append(type.getSimpleName())
				.append(" */ ")
				.append(metadata.getTableIdentifier())
				.append(" SET \n");

		for (int i = 0; i < columnSize; i++) {
			TableMetadata.Column col = columns.get(i);
			String camelColName = col.getCamelColumnName();

			sql.append("/*IF \"").append(camelColName).append("\" in getParameterNames() */\n");// フィールドがセットされていない場合はカラム自体を削る
			if (i > 0) {
				appendIfCommma(sql, columns, i);
			}
			sql.append(col.getColumnName()).append(" = /*").append(camelColName).append("*/''\n");
			sql.append("/*END*/\n");
		}
		sql.append("WHERE 1 = 1\n");
		for (TableMetadata.Column col : metadata.getKeyColumns()) {
			String camelColName = col.getCamelColumnName();
			sql.append("AND ").append(col.getColumnName()).append(" = ").append("/*").append(camelColName).append("*/''\n");
		}
		return sql.toString();
	}

	protected String buildDeleteSQL(final TableMetadata metadata, final Class<? extends Object> type) {
		StringBuilder sql = new StringBuilder("DELETE /* mapping @ ")
				.append(type.getSimpleName())
				.append(" */ FROM ")
				.append(metadata.getTableIdentifier())
				.append("\n");

		sql.append("WHERE 1 = 1\n");
		for (TableMetadata.Column col : metadata.getKeyColumns()) {
			String camelColName = col.getCamelColumnName();
			sql.append("AND ").append(col.getColumnName()).append(" = ").append("/*").append(camelColName).append("*/''\n");
		}
		return sql.toString();
	}

	private void appendIfCommma(final StringBuilder sql, final List<? extends TableMetadata.Column> columns, final int endColumnIndex) {
		sql.append("/*IF ");
		for (int i = 0; i < endColumnIndex; i++) {
			TableMetadata.Column col = columns.get(i);
			String camelColName = col.getCamelColumnName();
			if (i > 0) {
				sql.append(" || ");
			}
			sql.append("\"").append(camelColName).append("\" in getParameterNames()");
		}
		sql.append(" */\n, \n/*END*/\n");
	}

	private void setFields(final SqlContext context, final Object entity) {
		Class<?> type = entity.getClass();
		for (MappingColumn column : MappingUtils.getMappingColumns(type)) {
			Object value = column.getValue(entity);
			context.param(column.getCamelName(), value);
		}
	}
}
