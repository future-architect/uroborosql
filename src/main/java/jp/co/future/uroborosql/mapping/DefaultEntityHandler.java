/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionManager;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.EntityResultSetConverter;
import jp.co.future.uroborosql.enums.GenerationType;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.mapping.TableMetadata.Column;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapper;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * デフォルトORM処理クラス
 *
 * @author ota
 */
public class DefaultEntityHandler implements EntityHandler<Object> {

	private static Map<Class<?>, TableMetadata> CONTEXTS = new ConcurrentHashMap<>();
	private PropertyMapperManager propertyMapperManager;
	private boolean emptyStringEqualsNull = true;
	private SqlConfig sqlConfig = null;

	/**
	 * コンストラクタ
	 */
	public DefaultEntityHandler() {
		super();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#isEmptyStringEqualsNull()
	 */
	@Override
	public boolean isEmptyStringEqualsNull() {
		return emptyStringEqualsNull;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setEmptyStringEqualsNull(boolean)
	 */
	@Override
	public EntityHandler<Object> setEmptyStringEqualsNull(final boolean emptyStringEqualsNull) {
		this.emptyStringEqualsNull = emptyStringEqualsNull;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createSelectContext(jp.co.future.uroborosql.SqlAgent, jp.co.future.uroborosql.mapping.TableMetadata, java.lang.Class, boolean)
	 */
	@Override
	public SqlContext createSelectContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType, final boolean addCondition) {
		return agent.contextWith(buildSelectSQL(metadata, entityType, agent.getSqlConfig(), addCondition))
				.setSqlId(createSqlId(metadata, entityType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#doSelect(jp.co.future.uroborosql.SqlAgent, jp.co.future.uroborosql.context.SqlContext, java.lang.Class)
	 */
	@Override
	public <E> Stream<E> doSelect(final SqlAgent agent, final SqlContext context, final Class<? extends E> entityType)
			throws SQLException {
		return agent.query(context, new EntityResultSetConverter<>(entityType, propertyMapperManager));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createInsertContext(jp.co.future.uroborosql.SqlAgent, jp.co.future.uroborosql.mapping.TableMetadata, java.lang.Class)
	 */
	@Override
	public SqlContext createInsertContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.contextWith(buildInsertSQL(metadata, entityType, agent.getSqlConfig()))
				.setSqlId(createSqlId(metadata, entityType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createUpdateContext(jp.co.future.uroborosql.SqlAgent, jp.co.future.uroborosql.mapping.TableMetadata, java.lang.Class, boolean)
	 */
	@Override
	public SqlContext createUpdateContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType, final boolean addCondition) {
		return agent.contextWith(buildUpdateSQL(metadata, entityType, agent.getSqlConfig(), addCondition, true))
				.setSqlId(createSqlId(metadata, entityType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createDeleteContext(jp.co.future.uroborosql.SqlAgent, jp.co.future.uroborosql.mapping.TableMetadata, java.lang.Class, boolean)
	 */
	@Override
	public SqlContext createDeleteContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType, final boolean addCondition) {
		return agent.contextWith(buildDeleteSQL(metadata, entityType, agent.getSqlConfig(), addCondition))
				.setSqlId(createSqlId(metadata, entityType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createBatchInsertContext(SqlAgent, TableMetadata, Class)
	 */
	@Override
	public SqlContext createBatchInsertContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.contextWith(buildInsertSQL(metadata, entityType, agent.getSqlConfig(), false))
				.setSqlId(createSqlId(metadata, entityType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createBulkInsertContext(SqlAgent, TableMetadata, Class)
	 */
	@Override
	public SqlContext createBulkInsertContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.context().setSqlId(createSqlId(metadata, entityType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createBatchUpdateContext(jp.co.future.uroborosql.SqlAgent, jp.co.future.uroborosql.mapping.TableMetadata, java.lang.Class)
	 */
	@Override
	public SqlContext createBatchUpdateContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.contextWith(buildUpdateSQL(metadata, entityType, agent.getSqlConfig(), true, false))
				.setSqlId(createSqlId(metadata, entityType));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setupSqlBulkInsertContext(SqlAgent, SqlContext, TableMetadata, Class, int)
	 */
	@Override
	public SqlContext setupSqlBulkInsertContext(final SqlAgent agent, final SqlContext context,
			final TableMetadata metadata, final Class<? extends Object> entityType, final int numberOfRecords) {
		return context.setSql(buildBulkInsertSQL(metadata, entityType, agent.getSqlConfig(), numberOfRecords));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setInsertParams(jp.co.future.uroborosql.context.SqlContext, java.lang.Object)
	 */
	@Override
	public void setInsertParams(final SqlContext context, final Object entity) {
		setFields(context, entity, SqlKind.INSERT, MappingColumn::getCamelName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setUpdateParams(jp.co.future.uroborosql.context.SqlContext, java.lang.Object)
	 */
	@Override
	public void setUpdateParams(final SqlContext context, final Object entity) {
		setFields(context, entity, SqlKind.UPDATE, MappingColumn::getCamelName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setDeleteParams(jp.co.future.uroborosql.context.SqlContext, java.lang.Object)
	 */
	@Override
	public void setDeleteParams(final SqlContext context, final Object entity) {
		setFields(context, entity, SqlKind.DELETE, MappingColumn::getCamelName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setBulkInsertParams(jp.co.future.uroborosql.context.SqlContext, java.lang.Object, int)
	 */
	@Override
	public void setBulkInsertParams(final SqlContext context, final Object entity, final int entityIndex) {
		setFields(context, entity, SqlKind.INSERT, col -> buildBulkParamName(col.getCamelName(), entityIndex));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#getEntityType()
	 */
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

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#addPropertyMapper(jp.co.future.uroborosql.mapping.mapper.PropertyMapper)
	 */
	@Override
	public EntityHandler<Object> addPropertyMapper(final PropertyMapper<?> propertyMapper) {
		propertyMapperManager.addMapper(propertyMapper);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#removePropertyMapper(jp.co.future.uroborosql.mapping.mapper.PropertyMapper)
	 */
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
	protected TableMetadata createMetadata(final ConnectionManager connectionManager,
			final Class<? extends Object> type)
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
	 * @param sqlConfig SQLコンフィグ
	 * @param addCondition 条件を追加するかどうか。追加する場合<code>true</code>
	 * @return SELECT SQL
	 */
	protected String buildSelectSQL(final TableMetadata metadata, final Class<? extends Object> type,
			final SqlConfig sqlConfig, final boolean addCondition) {
		final List<? extends TableMetadata.Column> columns = metadata.getColumns();

		final StringBuilder sql = new StringBuilder(
				buildSelectClause(metadata, type, sqlConfig.getSqlAgentFactory().getSqlIdKeyName()));

		if (addCondition) {
			sql.append("/*BEGIN*/").append(System.lineSeparator());
			sql.append("WHERE").append(System.lineSeparator());

			for (final TableMetadata.Column col : columns) {
				final String camelColName = col.getCamelColumnName();
				final StringBuilder parts = new StringBuilder().append("\t").append("AND ")
						.append(col.getColumnIdentifier())
						.append(" = ").append("/*").append(camelColName).append("*/''").append(System.lineSeparator());
				wrapIfComment(sql, parts, col);
			}
			sql.append("/*END*/").append(System.lineSeparator());

			boolean firstFlag = true;
			final List<? extends TableMetadata.Column> keys = metadata.getKeyColumns();
			if (!keys.isEmpty()) {
				sql.append("ORDER BY").append(System.lineSeparator());
				firstFlag = true;
				for (final TableMetadata.Column col : keys) {
					sql.append("\t");
					if (firstFlag) {
						sql.append("  ");
						firstFlag = false;
					} else {
						sql.append(", ");
					}
					sql.append(col.getColumnIdentifier()).append(System.lineSeparator());
				}
			}
		}

		return sql.toString();
	}

	/**
	 * SELECT句生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @param sqlIdKeyName SQL_IDキー名
	 * @return SELECT句
	 */
	protected String buildSelectClause(final TableMetadata metadata, final Class<? extends Object> type,
			final String sqlIdKeyName) {
		final List<? extends TableMetadata.Column> columns = metadata.getColumns();

		final StringBuilder sql = new StringBuilder("SELECT ").append("/* ").append(sqlIdKeyName).append(" */")
				.append(System.lineSeparator());

		boolean firstFlag = true;
		for (final TableMetadata.Column col : columns) {
			sql.append("\t");
			if (firstFlag) {
				sql.append("  ");
				firstFlag = false;
			} else {
				sql.append(", ");
			}
			sql.append(col.getColumnIdentifier()).append("\tAS\t").append(col.getColumnIdentifier());
			if (StringUtils.isNotEmpty(col.getRemarks())) {
				sql.append("\t").append("-- ").append(col.getRemarks());
			}
			sql.append(System.lineSeparator());
		}
		sql.append("FROM ").append(metadata.getTableIdentifier()).append(System.lineSeparator());

		return sql.toString();
	}

	/**
	 * INSERT SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @param sqlConfig SQLコンフィグ
	 * @return INSERT SQL
	 */
	protected String buildInsertSQL(final TableMetadata metadata, final Class<? extends Object> type,
			final SqlConfig sqlConfig) {
		return buildInsertSQL(metadata, type, sqlConfig, true);
	}

	/**
	 * INSERT SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @param sqlConfig SQLコンフィグ
	 * @param ignoreWhenEmpty 空のパラメータをSQLに含めない条件文を設定する
	 * @return INSERT SQL
	 */
	protected String buildInsertSQL(final TableMetadata metadata, final Class<? extends Object> type,
			final SqlConfig sqlConfig, final boolean ignoreWhenEmpty) {
		Map<String, MappingColumn> mappingColumns = MappingUtils.getMappingColumnMap(type, SqlKind.INSERT);
		StringBuilder sql = buildInsertTargetBlock(metadata, mappingColumns, sqlConfig, ignoreWhenEmpty);
		sql.append(" VALUES ");
		sql.append(buildInsertRowBlock(metadata, mappingColumns, sqlConfig, ignoreWhenEmpty,
				TableMetadata.Column::getCamelColumnName));
		return sql.toString();
	}

	/**
	 * BULK INSERT SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @param sqlConfig SQLコンフィグ
	 * @param numberOfRecords レコード行数
	 * @return INSERT SQL
	 */
	protected String buildBulkInsertSQL(final TableMetadata metadata, final Class<? extends Object> type,
			final SqlConfig sqlConfig, final int numberOfRecords) {
		Map<String, MappingColumn> mappingColumns = MappingUtils.getMappingColumnMap(type, SqlKind.INSERT);
		StringBuilder sql = buildInsertTargetBlock(metadata, mappingColumns, sqlConfig, false);
		sql.append(" VALUES ");

		IntStream.range(0, numberOfRecords).forEach(i -> {
			if (i > 0) {
				sql.append(",").append(System.lineSeparator());
			}
			sql.append(buildInsertRowBlock(metadata, mappingColumns, sqlConfig, false,
					col -> buildBulkParamName(col.getCamelColumnName(), i)));
		});
		return sql.toString();
	}

	/**
	 * UPDATE SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @param sqlConfig SQLコンフィグ
	 * @param addCondition 条件を追加するかどうか。追加する場合<code>true</code>
	 * @param ignoreWhenEmpty 空のパラメータをSQLに含めない条件文を設定する
	 * @return UPDATE SQL
	 */
	protected String buildUpdateSQL(final TableMetadata metadata, final Class<? extends Object> type,
			final SqlConfig sqlConfig, final boolean addCondition, final boolean ignoreWhenEmpty) {
		StringBuilder sql = new StringBuilder("UPDATE ").append("/* ")
				.append(sqlConfig.getSqlAgentFactory().getSqlIdKeyName()).append(" */")
				.append(" ").append(metadata.getTableIdentifier()).append(" SET ").append(System.lineSeparator());

		Map<String, MappingColumn> mappingColumns = MappingUtils.getMappingColumnMap(type, SqlKind.UPDATE);

		String versionColumnName = null;
		OptimisticLockSupplier optimisticLockSupplier = null;
		if (type == null) {
			TableMetadata.Column versionColumn = metadata.getColumns().stream().filter(Column::isVersion).findFirst()
					.orElse(null);
			if (versionColumn != null) {
				versionColumnName = versionColumn.getCamelColumnName();
				optimisticLockSupplier = OptimisticLockSupplier.getSupplier(versionColumn.getOptimisticLockType());
			}
		} else {
			Optional<MappingColumn> versionColumn = MappingUtils.getVersionMappingColumn(type);
			versionColumnName = versionColumn.map(MappingColumn::getCamelName).orElse(null);
			optimisticLockSupplier = versionColumn
					.map(vc -> OptimisticLockSupplier.getSupplier(vc.getVersion().supplier())).orElse(null);
		}
		boolean firstFlag = true;
		for (TableMetadata.Column col : metadata.getColumns()) {
			if (!mappingColumns.isEmpty()) {
				MappingColumn mappingColumn = mappingColumns.get(col.getCamelColumnName());
				if (mappingColumn == null) {
					// Transient annotation のついているカラムをスキップ
					continue;
				} else if (mappingColumn.isId()) {
					// @Idが付与されたカラムは自動採番なので更新対象としないためスキップする
					continue;
				}
			} else {
				if (col.isAutoincrement()) {
					// 自動採番カラムは更新対象としないためスキップする
					continue;
				}
			}

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

			boolean isVersionColumn = camelColName.equalsIgnoreCase(versionColumnName);
			if (isVersionColumn) {
				parts.append(optimisticLockSupplier.getPart(col, getSqlConfig()));
			} else {
				parts.append(col.getColumnIdentifier());
				parts.append(" = /*").append(camelColName).append("*/''");
			}
			if (StringUtils.isNotEmpty(col.getRemarks())) {
				parts.append("\t").append("-- ").append(col.getRemarks());
			}
			parts.append(System.lineSeparator());
			if (isVersionColumn) {
				sql.append(parts);
			} else if (addCondition) {
				if (ignoreWhenEmpty && col.isNullable()) {
					wrapIfComment(sql, parts, col);
				} else {
					sql.append(parts);
				}
			} else {
				if (ignoreWhenEmpty) {
					wrapIfComment(sql, parts, col);
				} else {
					sql.append(parts);
				}
			}
		}

		if (addCondition) {
			sql.append("WHERE").append(System.lineSeparator());
			final List<? extends Column> cols = !metadata.getKeyColumns().isEmpty() ? metadata.getKeyColumns()
					: Arrays.asList(metadata.getColumns().get(0));
			firstFlag = true;
			for (final TableMetadata.Column col : cols) {
				final StringBuilder parts = new StringBuilder().append("\t");
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
				parts.append(col.getColumnIdentifier()).append(" = ").append("/*").append(col.getCamelColumnName())
						.append("*/''")
						.append(System.lineSeparator());
				if (ignoreWhenEmpty && col.isNullable()) {
					wrapIfComment(sql, parts, col);
				} else {
					sql.append(parts);
				}
			}
			final boolean first = firstFlag;
			if (versionColumnName != null) {
				TableMetadata.Column col = metadata.getColumn(versionColumnName);
				sql.append("\t");
				if (first) {
					sql.append("    ");
				} else {
					sql.append("AND ");
				}
				sql.append(col.getColumnIdentifier()).append(" = ").append("/*").append(col.getCamelColumnName())
						.append("*/''").append(System.lineSeparator());
			}
		}
		return sql.toString();
	}

	protected void addVersionColumn(final StringBuilder parts, final TableMetadata.Column col) {
		parts.append(col.getColumnIdentifier());
		parts.append(" = ").append(col.getColumnIdentifier()).append(" + 1");
	}

	/**
	 * DELETE SQL生成
	 *
	 * @param metadata エンティティメタ情報
	 * @param type エイティティタイプ
	 * @param sqlConfig SQLコンフィグ
	 * @param addCondition 条件を追加するかどうか。追加する場合<code>true</code>
	 * @return DELETE SQL
	 */
	protected String buildDeleteSQL(final TableMetadata metadata, final Class<? extends Object> type,
			final SqlConfig sqlConfig, final boolean addCondition) {
		StringBuilder sql = new StringBuilder("DELETE ").append("/* ")
				.append(sqlConfig.getSqlAgentFactory().getSqlIdKeyName()).append(" */")
				.append(" FROM ").append(metadata.getTableIdentifier()).append("").append(System.lineSeparator());

		if (addCondition) {
			boolean firstFlag = true;
			sql.append("WHERE").append(System.lineSeparator());

			List<? extends Column> cols = !metadata.getKeyColumns().isEmpty() ? metadata.getKeyColumns()
					: Arrays
							.asList(metadata.getColumns().get(0));
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
				parts.append(col.getColumnIdentifier()).append(" = ").append("/*").append(col.getCamelColumnName())
						.append("*/''").append(System.lineSeparator());
				if (col.isNullable()) {
					wrapIfComment(sql, parts, col);
				} else {
					sql.append(parts);
				}
			}
		}
		return sql.toString();
	}

	private StringBuilder buildInsertTargetBlock(final TableMetadata metadata,
			final Map<String, MappingColumn> mappingColumns,
			final SqlConfig sqlConfig, final boolean ignoreWhenEmpty) {

		StringBuilder sql = new StringBuilder("INSERT ").append("/* ")
				.append(sqlConfig.getSqlAgentFactory().getSqlIdKeyName()).append(" */")
				.append(" INTO ").append(metadata.getTableIdentifier()).append(" (").append(System.lineSeparator());

		boolean firstFlag = true;
		for (TableMetadata.Column col : metadata.getColumns()) {
			MappingColumn mappingColumn = mappingColumns.get(col.getCamelColumnName());
			if (!mappingColumns.isEmpty() && mappingColumn == null) {
				// Transient annotation のついているカラムをスキップ
				continue;
			}
			boolean autoIncrementColumn = mappingColumn != null && mappingColumn.isId()
					&& GenerationType.IDENTITY.equals(mappingColumn.getGeneratedValue().strategy()) ||
					col.isAutoincrement();

			StringBuilder parts = new StringBuilder().append("\t");
			if (firstFlag) {
				if (col.isNullable() || autoIncrementColumn) {
					parts.append(", ");
				} else {
					parts.append("  ");
				}
				firstFlag = false;
			} else {
				parts.append(", ");
			}

			parts.append(col.getColumnIdentifier());
			if (StringUtils.isNotEmpty(col.getRemarks())) {
				parts.append("\t").append("-- ").append(col.getRemarks());
			}
			parts.append(System.lineSeparator());
			if (ignoreWhenEmpty && col.isNullable() || autoIncrementColumn) {
				wrapIfComment(sql, parts, col);
			} else {
				sql.append(parts);
			}
		}

		sql.append(")");
		return sql;
	}

	private StringBuilder buildInsertRowBlock(final TableMetadata metadata,
			final Map<String, MappingColumn> mappingColumns,
			final SqlConfig sqlConfig, final boolean ignoreWhenEmpty,
			final Function<TableMetadata.Column, String> getParamName) {
		StringBuilder sql = new StringBuilder("(").append(System.lineSeparator());
		boolean firstFlag = true;
		for (TableMetadata.Column col : metadata.getColumns()) {
			MappingColumn mappingColumn = mappingColumns.get(col.getCamelColumnName());

			if (!mappingColumns.isEmpty() && mappingColumn == null) {
				// Transient annotation のついているカラムをスキップ
				continue;
			}
			boolean autoIncrementColumn = mappingColumn != null && mappingColumn.isId()
					&& GenerationType.IDENTITY.equals(mappingColumn.getGeneratedValue().strategy()) ||
					col.isAutoincrement();

			StringBuilder parts = new StringBuilder().append("\t");
			if (firstFlag) {
				if (col.isNullable() || autoIncrementColumn) {
					parts.append(", ");
				} else {
					parts.append("  ");
				}
				firstFlag = false;
			} else {
				parts.append(", ");
			}

			if (mappingColumn != null && mappingColumn.isId()
					&& GenerationType.SEQUENCE.equals(mappingColumn.getGeneratedValue().strategy())) {
				String sequenceName = mappingColumn.getQualifiedSequenceName();
				parts.append(sqlConfig.getDialect().getSequenceNextValSql(sequenceName)).append(System.lineSeparator());
				sql.append(parts);
			} else {
				parts.append("/*").append(getParamName.apply(col)).append("*/''").append(System.lineSeparator());
				if (ignoreWhenEmpty && col.isNullable() || autoIncrementColumn) {
					wrapIfComment(sql, parts, col);
				} else {
					sql.append(parts);
				}
			}
		}

		sql.append(")");
		return sql;
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
	private boolean isStringType(final int type) {
		return JDBCType.CHAR.getVendorTypeNumber().equals(type) || JDBCType.NCHAR.getVendorTypeNumber().equals(type)
				|| JDBCType.VARCHAR.getVendorTypeNumber().equals(type)
				|| JDBCType.NVARCHAR.getVendorTypeNumber().equals(type)
				|| JDBCType.LONGNVARCHAR.getVendorTypeNumber().equals(type);
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
		// フィールドがセットされていない場合はカラム自体を削る
		if (isStringType(col.getDataType())) {
			if (emptyStringEqualsNull) {
				original.append("/*IF SF.isNotEmpty(").append(camelColName).append(") */")
						.append(System.lineSeparator());
			} else {
				original.append("/*IF ").append(camelColName).append(" != null */").append(System.lineSeparator());
			}
		} else {
			original.append("/*IF ").append(camelColName).append(" != null */").append(System.lineSeparator());
		}
		original.append(addParts);
		original.append("/*END*/").append(System.lineSeparator());
		return original;
	}

	private void setFields(final SqlContext context, final Object entity, final SqlKind kind,
			final Function<MappingColumn, String> getParamName) {
		List<String> generatedKeyColumns = new ArrayList<>();
		if (context.getGeneratedKeyColumns() != null) {
			for (String keyColumn : context.getGeneratedKeyColumns()) {
				generatedKeyColumns.add(CaseFormat.CAMEL_CASE.convert(keyColumn));
			}
		}
		for (MappingColumn column : MappingUtils.getMappingColumns(entity.getClass(), kind)) {
			if (SqlKind.INSERT.equals(kind) && generatedKeyColumns.contains(column.getCamelName())) {
				continue;
			}

			context.param(getParamName.apply(column), column.getValue(entity));
		}
	}

	private String buildBulkParamName(final String base, final int entityIndex) {
		return base + "$" + entityIndex;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#initialize()
	 */
	@Override
	public void initialize() {
		this.propertyMapperManager = new PropertyMapperManager(this.sqlConfig.getClock());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfigAware#setSqlConfig(jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public void setSqlConfig(final SqlConfig sqlConfig) {
		this.sqlConfig = sqlConfig;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfigAware#getSqlConfig()
	 */
	@Override
	public SqlConfig getSqlConfig() {
		return this.sqlConfig;
	}

}
