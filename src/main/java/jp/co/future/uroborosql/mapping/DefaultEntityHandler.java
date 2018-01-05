package jp.co.future.uroborosql.mapping;

import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.connection.ConnectionManager;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.EntityResultSetConverter;
import jp.co.future.uroborosql.mapping.TableMetadata.Column;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapper;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;

import org.apache.commons.lang3.StringUtils;

/**
 * デフォルトORM処理クラス
 *
 * @author ota
 */
public class DefaultEntityHandler implements EntityHandler<Object> {

	private static Map<Class<?>, TableMetadata> CONTEXTS = new ConcurrentHashMap<>();
	private final PropertyMapperManager propertyMapperManager = new PropertyMapperManager();

	@Override
	public SqlContext createSelectContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.contextWith(buildSelectSQL(metadata, entityType, agent.getSqlConfig().getSqlAgentFactory()
				.getSqlIdKeyName())).setSqlId(createSqlId(metadata, entityType));
	}

	@Override
	public <E> Stream<E> doSelect(final SqlAgent agent, final SqlContext context, final Class<? extends E> entityType)
			throws SQLException {
		return agent.query(context, new EntityResultSetConverter<>(entityType, new PropertyMapperManager(
				propertyMapperManager)));
	}

	@Override
	public SqlContext createInsertContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.contextWith(buildInsertSQL(metadata, entityType, agent.getSqlConfig().getSqlAgentFactory()
				.getSqlIdKeyName())).setSqlId(createSqlId(metadata, entityType));
	}

	@Override
	public SqlContext createUpdateContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.contextWith(buildUpdateSQL(metadata, entityType, agent.getSqlConfig().getSqlAgentFactory()
				.getSqlIdKeyName())).setSqlId(createSqlId(metadata, entityType));
	}

	@Override
	public SqlContext createDeleteContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.contextWith(buildDeleteSQL(metadata, entityType, agent.getSqlConfig().getSqlAgentFactory()
				.getSqlIdKeyName())).setSqlId(createSqlId(metadata, entityType));
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
	public TableMetadata getMetadata(final ConnectionManager connectionManager, final Class<?> entityType)
			throws SQLException {
		TableMetadata context = CONTEXTS.get(entityType);
		if (context == null) {
			context = createMetadata(connectionManager, entityType);
			CONTEXTS.put(entityType, context);
		}
		return context;
	}

	@Override
	public EntityHandler<Object> addPropertyMapper(final PropertyMapper<?> propertyMapper) {
		propertyMapperManager.addMapper(propertyMapper);
		return this;
	}

	@Override
	public EntityHandler<Object> removePropertyMapper(final PropertyMapper<?> propertyMapper) {
		propertyMapperManager.removeMapper(propertyMapper);
		return this;
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
		Table table = getTable(type);
		return TableMetadata.createTableEntityMetadata(connectionManager, table);
	}

	/**
	 * エンティティ型からテーブル情報の取得
	 *
	 * @param type エンティティ型
	 * @return テーブル情報
	 */
	protected Table getTable(final Class<? extends Object> type) {
		return MappingUtils.getTable(type);
	}

	/**
	 * SELECT SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @param sqlIdKeyName SQL_IDキー名
	 * @return SELECT SQL
	 */
	protected String buildSelectSQL(final TableMetadata metadata, final Class<? extends Object> type,
			final String sqlIdKeyName) {
		List<? extends TableMetadata.Column> columns = metadata.getColumns();

		StringBuilder sql = new StringBuilder("SELECT ").append("/* ").append(sqlIdKeyName).append(" */")
				.append(System.lineSeparator());

		boolean firstFlag = true;
		for (TableMetadata.Column col : columns) {
			sql.append("\t");
			if (firstFlag) {
				sql.append("  ");
				firstFlag = false;
			} else {
				sql.append(", ");
			}
			sql.append(col.getColumnName()).append("\tAS\t").append(col.getColumnName());
			if (StringUtils.isNotEmpty(col.getRemarks())) {
				sql.append("\t").append("-- ").append(col.getRemarks());
			}
			sql.append(System.lineSeparator());
		}
		sql.append("FROM ").append(metadata.getTableIdentifier()).append(System.lineSeparator());
		sql.append("/*BEGIN*/").append(System.lineSeparator());
		sql.append("WHERE").append(System.lineSeparator());

		for (TableMetadata.Column col : columns) {
			String camelColName = col.getCamelColumnName();
			StringBuilder parts = new StringBuilder().append("\t").append("AND ").append(col.getColumnName())
					.append(" = ").append("/*").append(camelColName).append("*/''").append(System.lineSeparator());
			wrapIfComment(sql, parts, col);
		}
		sql.append("/*END*/").append(System.lineSeparator());

		List<? extends TableMetadata.Column> keys = metadata.getKeyColumns();
		if (!keys.isEmpty()) {
			sql.append("ORDER BY ").append(System.lineSeparator());
			firstFlag = true;
			for (TableMetadata.Column col : keys) {
				sql.append("\t");
				if (firstFlag) {
					sql.append("  ");
					firstFlag = false;
				} else {
					sql.append(", ");
				}
				sql.append(col.getColumnName()).append(System.lineSeparator());
			}
		}

		return sql.toString();
	}

	/**
	 * INSERT SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @param sqlIdKeyName SQL_IDキー名
	 * @return INSERT SQL
	 */
	protected String buildInsertSQL(final TableMetadata metadata, final Class<? extends Object> type,
			final String sqlIdKeyName) {
		StringBuilder sql = new StringBuilder("INSERT ").append("/* ").append(sqlIdKeyName).append(" */")
				.append(" INTO ").append(metadata.getTableIdentifier()).append("(").append(System.lineSeparator());

		boolean firstFlag = true;
		for (TableMetadata.Column col : metadata.getColumns()) {
			StringBuilder parts = new StringBuilder().append("\t");
			if (firstFlag) {
				if (col.isNullable()) {
					parts.append(", ");
				} else {
					parts.append("  ");
				}
				firstFlag = false;
			} else {
				parts.append(", ");
			}
			parts.append(col.getColumnName());
			if (StringUtils.isNotEmpty(col.getRemarks())) {
				parts.append("\t").append("-- ").append(col.getRemarks());
			}
			parts.append(System.lineSeparator());
			if (col.isNullable()) {
				wrapIfComment(sql, parts, col);
			} else {
				sql.append(parts);
			}
		}

		sql.append(") VALUES (").append(System.lineSeparator());

		firstFlag = true;
		for (TableMetadata.Column col : metadata.getColumns()) {
			StringBuilder parts = new StringBuilder().append("\t");
			if (firstFlag) {
				if (col.isNullable()) {
					parts.append(", ");
				} else {
					parts.append("  ");
				}
				firstFlag = false;
			} else {
				parts.append(", ");
			}
			parts.append("/*").append(col.getCamelColumnName()).append("*/''").append(System.lineSeparator());
			if (col.isNullable()) {
				wrapIfComment(sql, parts, col);
			} else {
				sql.append(parts);
			}
		}

		sql.append(")");
		return sql.toString();
	}

	/**
	 * UPDATE SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @param sqlIdKeyName SQL_IDキー名
	 * @return UPDATE SQL
	 */
	protected String buildUpdateSQL(final TableMetadata metadata, final Class<? extends Object> type,
			final String sqlIdKeyName) {
		StringBuilder sql = new StringBuilder("UPDATE ").append("/* ").append(sqlIdKeyName).append(" */")
				.append(" ").append(metadata.getTableIdentifier()).append(" SET ").append(System.lineSeparator());

		Optional<MappingColumn> versionMappingColumn = type == null ? Optional.empty() : MappingUtils
				.getVersionMappingColumn(type);

		boolean firstFlag = true;
		for (TableMetadata.Column col : metadata.getColumns()) {
			String camelColName = col.getCamelColumnName();
			StringBuilder parts = new StringBuilder().append("\t");
			if (firstFlag) {
				if (col.isNullable()) {
					parts.append(", ");
				} else {
					parts.append("  ");
				}
				firstFlag = false;
			} else {
				parts.append(", ");
			}
			parts.append(col.getColumnName()).append(" = /*").append(camelColName).append("*/''");
			versionMappingColumn.ifPresent(mappingColumn -> {
				if (camelColName.equals(mappingColumn.getCamelName())) {
					parts.append(" + 1");
				}
			});
			if (StringUtils.isNotEmpty(col.getRemarks())) {
				parts.append("\t").append("-- ").append(col.getRemarks());
			}
			parts.append(System.lineSeparator());
			if (col.isNullable()) {
				wrapIfComment(sql, parts, col);
			} else {
				sql.append(parts);
			}
		}

		sql.append("WHERE").append(System.lineSeparator());
		List<? extends Column> cols = !metadata.getKeyColumns().isEmpty() ? metadata.getKeyColumns() : metadata
				.getColumns();
		firstFlag = true;
		for (TableMetadata.Column col : cols) {
			StringBuilder parts = new StringBuilder().append("\t");
			if (firstFlag) {
				if (col.isNullable()) {
					parts.append("AND ");
				} else {
					parts.append("    ");
				}
				firstFlag = false;
			} else {
				parts.append("AND ");
			}
			parts.append(col.getColumnName()).append(" = ").append("/*").append(col.getCamelColumnName())
					.append("*/''")
					.append(System.lineSeparator());
			if (col.isNullable()) {
				wrapIfComment(sql, parts, col);
			} else {
				sql.append(parts);
			}
		}
		final boolean first = firstFlag;
		versionMappingColumn.ifPresent(mappingColumn -> {
			sql.append("\t");
			if (first) {
				sql.append("    ");
			} else {
				sql.append("AND ");
			}
			sql.append(mappingColumn.getName()).append(" = ").append("/*").append(mappingColumn.getCamelName())
					.append("*/''").append(System.lineSeparator());
		});
		return sql.toString();
	}

	/**
	 * DELETE SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @param sqlIdKeyName SQL_IDキー名
	 * @return DELETE SQL
	 */
	protected String buildDeleteSQL(final TableMetadata metadata, final Class<? extends Object> type,
			final String sqlIdKeyName) {
		StringBuilder sql = new StringBuilder("DELETE ").append("/* ").append(sqlIdKeyName).append(" */")
				.append(" FROM ").append(metadata.getTableIdentifier()).append("").append(System.lineSeparator());

		boolean firstFlag = true;
		sql.append("WHERE").append(System.lineSeparator());

		List<? extends Column> cols = !metadata.getKeyColumns().isEmpty() ? metadata.getKeyColumns() : metadata
				.getColumns();
		for (TableMetadata.Column col : cols) {
			StringBuilder parts = new StringBuilder().append("\t");
			if (firstFlag) {
				if (col.isNullable()) {
					parts.append("AND ");
				} else {
					parts.append("    ");
				}
				firstFlag = false;
			} else {
				parts.append("AND ");
			}
			parts.append(col.getColumnName()).append(" = ").append("/*").append(col.getCamelColumnName())
					.append("*/''").append(System.lineSeparator());
			if (col.isNullable()) {
				wrapIfComment(sql, parts, col);
			} else {
				sql.append(parts);
			}
		}
		return sql.toString();
	}

	/**
	 * SQL_ID文字列の生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param entityType エイティティタイプ
	 * @return SQL_ID文字列
	 */
	private String createSqlId(final TableMetadata metadata, final Class<? extends Object> entityType) {
		return "mapping @ " + (entityType != null ? entityType.getSimpleName() : metadata.getTableName());
	}

	/**
	 * 文字列型かどうかを判定する
	 *
	 * @param type JDBC上の型
	 * @return 文字列型の場合<code>true</code>
	 */
	private boolean isStringType(final JDBCType type) {
		return (JDBCType.CHAR.equals(type) || JDBCType.NCHAR.equals(type) || JDBCType.VARCHAR.equals(type)
				|| JDBCType.NVARCHAR.equals(type) || JDBCType.LONGNVARCHAR.equals(type));
	}

	/**
	 * IFコメントでSQLをラップする
	 *
	 * @param original SQL
	 * @param addParts IFコメントの中に含まれるSQLパーツ
	 * @param col カラム情報
	 * @return SQL
	 */
	private StringBuilder wrapIfComment(final StringBuilder original, final StringBuilder addParts,
			final TableMetadata.Column col) {
		String camelColName = col.getCamelColumnName();
		if (isStringType(col.getDataType())) {
			original.append("/*IF SF.isNotEmpty(").append(camelColName).append(") */").append(System.lineSeparator());// フィールドがセットされていない場合はカラム自体を削る
		} else {
			original.append("/*IF ").append(camelColName).append(" != null */").append(System.lineSeparator());// フィールドがセットされていない場合はカラム自体を削る
		}
		original.append(addParts);
		original.append("/*END*/").append(System.lineSeparator());
		return original;
	}

	private void setFields(final SqlContext context, final Object entity) {
		Class<?> type = entity.getClass();
		for (MappingColumn column : MappingUtils.getMappingColumns(type)) {
			Object value = column.getValue(entity);
			context.param(column.getCamelName(), value);
		}
	}
}
