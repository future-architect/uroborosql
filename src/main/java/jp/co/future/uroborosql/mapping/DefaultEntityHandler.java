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
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionManager;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.converter.EntityResultSetConverter;
import jp.co.future.uroborosql.enums.GenerationType;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
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
	/** TableMetadataのキャッシュサイズ. */
	protected static final int CACHE_SIZE = Integer.getInteger("uroborosql.entity.cache.size", 30);
	/** TableMetadataのLRUキャッシュ. */
	protected static final ConcurrentLruCache<String, TableMetadata> CACHE = new ConcurrentLruCache<>(CACHE_SIZE);
	/** プロパティマッパーマネージャー. */
	protected PropertyMapperManager propertyMapperManager;
	/** 空文字をNULLとして扱うか. */
	protected boolean emptyStringEqualsNull = true;
	/** SQLコンフィグ. */
	protected SqlConfig sqlConfig = null;

	/**
	 * コンストラクタ
	 */
	public DefaultEntityHandler() {
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
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createSelectContext(SqlAgent, TableMetadata, Class, boolean)
	 */
	@Override
	public ExecutionContext createSelectContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType, final boolean addCondition) {
		return agent.context().setSql(buildSelectSQL(metadata, entityType, agent.getSqlConfig(), addCondition))
				.setSqlId(createSqlId(metadata, entityType))
				.setSchema(metadata.getSchema())
				.setSqlName(entityType != null ? entityType.getCanonicalName() : null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#doSelect(SqlAgent, ExecutionContext, Class)
	 */
	@Override
	public <E> Stream<E> doSelect(final SqlAgent agent, final ExecutionContext context,
			final Class<? extends E> entityType)
			throws SQLException {
		return agent.query(context,
				new EntityResultSetConverter<>(context.getSchema(), entityType, propertyMapperManager));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createInsertContext(SqlAgent, TableMetadata, Class)
	 */
	@Override
	public ExecutionContext createInsertContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.context().setSql(buildInsertSQL(metadata, entityType, agent.getSqlConfig()))
				.setSqlId(createSqlId(metadata, entityType))
				.setSchema(metadata.getSchema())
				.setSqlName(entityType != null ? entityType.getCanonicalName() : null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createUpdateContext(SqlAgent, TableMetadata, Class, boolean)
	 */
	@Override
	public ExecutionContext createUpdateContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType, final boolean addCondition) {
		return agent.context().setSql(buildUpdateSQL(metadata, entityType, agent.getSqlConfig(), addCondition, true))
				.setSqlId(createSqlId(metadata, entityType))
				.setSchema(metadata.getSchema())
				.setSqlName(entityType != null ? entityType.getCanonicalName() : null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createDeleteContext(SqlAgent, TableMetadata, Class, boolean)
	 */
	@Override
	public ExecutionContext createDeleteContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType, final boolean addCondition) {
		return agent.context().setSql(buildDeleteSQL(metadata, entityType, agent.getSqlConfig(), addCondition))
				.setSqlId(createSqlId(metadata, entityType))
				.setSchema(metadata.getSchema())
				.setSqlName(entityType != null ? entityType.getCanonicalName() : null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createBatchInsertContext(SqlAgent, TableMetadata, Class)
	 */
	@Override
	public ExecutionContext createBatchInsertContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.context().setSql(buildInsertSQL(metadata, entityType, agent.getSqlConfig(), false))
				.setSqlId(createSqlId(metadata, entityType))
				.setSchema(metadata.getSchema())
				.setSqlName(entityType != null ? entityType.getCanonicalName() : null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createBulkInsertContext(SqlAgent, TableMetadata, Class)
	 */
	@Override
	public ExecutionContext createBulkInsertContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.context()
				.setSqlId(createSqlId(metadata, entityType))
				.setSchema(metadata.getSchema())
				.setSqlName(entityType != null ? entityType.getCanonicalName() : null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#createBatchUpdateContext(SqlAgent, TableMetadata, Class)
	 */
	@Override
	public ExecutionContext createBatchUpdateContext(final SqlAgent agent, final TableMetadata metadata,
			final Class<? extends Object> entityType) {
		return agent.context().setSql(buildUpdateSQL(metadata, entityType, agent.getSqlConfig(), true, false))
				.setSqlId(createSqlId(metadata, entityType))
				.setSchema(metadata.getSchema())
				.setSqlName(entityType != null ? entityType.getCanonicalName() : null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setupSqlBulkInsertContext(SqlAgent, ExecutionContext, TableMetadata, Class, int)
	 */
	@Override
	public ExecutionContext setupSqlBulkInsertContext(final SqlAgent agent, final ExecutionContext context,
			final TableMetadata metadata, final Class<? extends Object> entityType, final int numberOfRecords) {
		return context.setSql(buildBulkInsertSQL(metadata, entityType, agent.getSqlConfig(), numberOfRecords))
				.setSchema(metadata.getSchema())
				.setSqlName(entityType != null ? entityType.getCanonicalName() : null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setInsertParams(ExecutionContext, Object)
	 */
	@Override
	public void setInsertParams(final ExecutionContext context, final Object entity) {
		setFields(context, entity, SqlKind.INSERT, MappingColumn::getCamelName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setUpdateParams(ExecutionContext, Object)
	 */
	@Override
	public void setUpdateParams(final ExecutionContext context, final Object entity) {
		setFields(context, entity, SqlKind.UPDATE, MappingColumn::getCamelName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setDeleteParams(ExecutionContext, Object)
	 */
	@Override
	public void setDeleteParams(final ExecutionContext context, final Object entity) {
		setFields(context, entity, SqlKind.DELETE, MappingColumn::getCamelName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.EntityHandler#setBulkInsertParams(ExecutionContext, Object, int)
	 */
	@Override
	public void setBulkInsertParams(final ExecutionContext context, final Object entity, final int entityIndex) {
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
		var cacheKey = getCacheKey(connectionManager, entityType);
		return CACHE.get(cacheKey, key -> {
			try {
				return createMetadata(connectionManager, entityType);
			} catch (SQLException e) {
				throw new UroborosqlSQLException(e);
			}
		});
	}

	private String getCacheKey(final ConnectionManager connectionManager, final Class<?> entityType) {
		try {
			var table = MappingUtils.getTable(entityType);
			var schema = StringUtils.isNotEmpty(table.getSchema()) ? table.getSchema()
					: Objects.toString(connectionManager.getConnection().getSchema(), "");
			return String.format("%s.%s", schema.toUpperCase(), entityType.getName());
		} catch (SQLException ex) {
			return entityType.getName();
		}
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
		var table = getTable(type);
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
		var columns = metadata.getColumns();

		var sql = new StringBuilder(
				buildSelectClause(metadata, type, sqlConfig.getSqlAgentProvider().getSqlIdKeyName()));

		if (addCondition) {
			sql.append("/*BEGIN*/").append(System.lineSeparator());
			sql.append("WHERE").append(System.lineSeparator());

			for (var col : columns) {
				var camelColName = col.getCamelColumnName();
				var parts = new StringBuilder().append("\t").append("AND ")
						.append(col.getColumnIdentifier())
						.append(" = ").append("/*").append(camelColName).append("*/''").append(System.lineSeparator());
				wrapIfComment(sql, parts, col);
			}
			sql.append("/*END*/").append(System.lineSeparator());

			var firstFlag = true;
			var keys = metadata.getKeyColumns();
			if (!keys.isEmpty()) {
				sql.append("ORDER BY").append(System.lineSeparator());
				firstFlag = true;
				for (var col : keys) {
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
		var columns = metadata.getColumns();

		var sql = new StringBuilder("SELECT ").append("/* ").append(sqlIdKeyName).append(" */")
				.append(System.lineSeparator());

		var firstFlag = true;
		for (var col : columns) {
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
		var mappingColumns = MappingUtils.getMappingColumnMap(metadata.getSchema(), type, SqlKind.INSERT);
		var sql = buildInsertTargetBlock(metadata, mappingColumns, sqlConfig, ignoreWhenEmpty);
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
		var mappingColumns = MappingUtils.getMappingColumnMap(metadata.getSchema(), type, SqlKind.INSERT);
		var sql = buildInsertTargetBlock(metadata, mappingColumns, sqlConfig, false);
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
		var sql = new StringBuilder("UPDATE ").append("/* ")
				.append(sqlConfig.getSqlAgentProvider().getSqlIdKeyName()).append(" */")
				.append(" ").append(metadata.getTableIdentifier()).append(" SET ").append(System.lineSeparator());

		var mappingColumns = MappingUtils.getMappingColumnMap(metadata.getSchema(), type, SqlKind.UPDATE);

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
			var versionColumn = MappingUtils.getVersionMappingColumn(metadata.getSchema(), type);
			versionColumnName = versionColumn.map(MappingColumn::getCamelName).orElse(null);
			optimisticLockSupplier = versionColumn
					.map(vc -> OptimisticLockSupplier.getSupplier(vc.getVersion().supplier())).orElse(null);
		}

		var firstFlag = true;
		for (var col : metadata.getColumns()) {
			var mappingColumn = mappingColumns.get(col.getCamelColumnName());
			var autoIncrementColumn = mappingColumn != null && mappingColumn.isId() || col.isAutoincrement();

			if (!mappingColumns.isEmpty() && mappingColumn == null || addCondition && autoIncrementColumn) {
				// Transient annotation のついているカラムはスキップ、または、WHERE条件を追加する場合、自動採番カラムは更新対象としないためスキップ
				continue;
			}

			var camelColName = col.getCamelColumnName();
			var parts = new StringBuilder().append("\t");
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

			var isVersionColumn = camelColName.equalsIgnoreCase(versionColumnName);
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
				if (ignoreWhenEmpty || autoIncrementColumn) {
					wrapIfComment(sql, parts, col);
				} else {
					sql.append(parts);
				}
			}
		}

		if (addCondition) {
			sql.append("WHERE").append(System.lineSeparator());
			var cols = !metadata.getKeyColumns().isEmpty() ? metadata.getKeyColumns()
					: List.of(metadata.getColumns().get(0));
			firstFlag = true;
			for (final TableMetadata.Column col : cols) {
				var parts = new StringBuilder().append("\t");
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
			var first = firstFlag;
			if (versionColumnName != null) {
				var col = metadata.getColumn(versionColumnName);
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
		var sql = new StringBuilder("DELETE ").append("/* ")
				.append(sqlConfig.getSqlAgentProvider().getSqlIdKeyName()).append(" */")
				.append(" FROM ").append(metadata.getTableIdentifier()).append("").append(System.lineSeparator());

		if (addCondition) {
			var firstFlag = true;
			sql.append("WHERE").append(System.lineSeparator());

			var cols = !metadata.getKeyColumns().isEmpty() ? metadata.getKeyColumns()
					: List.of(metadata.getColumns().get(0));
			for (var col : cols) {
				var parts = new StringBuilder().append("\t");
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

	/**
	 * INSERT文の中で 挿入対象カラムを構成するブロックを生成する.
	 *
	 * @param metadata エンティティメタ情報
	 * @param mappingColumns マッピングカラム情報
	 * @param sqlConfig SQLコンフィグ
	 * @param ignoreWhenEmpty 空の場合無視するかどうか
	 * @return 生成したSQLパーツ
	 */
	protected StringBuilder buildInsertTargetBlock(final TableMetadata metadata,
			final Map<String, MappingColumn> mappingColumns,
			final SqlConfig sqlConfig, final boolean ignoreWhenEmpty) {

		var sql = new StringBuilder("INSERT ").append("/* ")
				.append(sqlConfig.getSqlAgentProvider().getSqlIdKeyName()).append(" */")
				.append(" INTO ").append(metadata.getTableIdentifier()).append(" (").append(System.lineSeparator());

		var firstFlag = true;
		for (var col : metadata.getColumns()) {
			var mappingColumn = mappingColumns.get(col.getCamelColumnName());
			if (!mappingColumns.isEmpty() && mappingColumn == null) {
				// Transient annotation のついているカラムをスキップ
				continue;
			}
			var autoIncrementColumn = mappingColumn != null && mappingColumn.isId()
					&& GenerationType.IDENTITY.equals(mappingColumn.getGeneratedValue().strategy()) ||
					col.isAutoincrement();

			var parts = new StringBuilder().append("\t");
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
			if (ignoreWhenEmpty && (col.isNullable() || StringUtils.isNotEmpty(col.getColumnDefault()))
					|| autoIncrementColumn) {
				wrapIfComment(sql, parts, col);
			} else {
				sql.append(parts);
			}
		}

		sql.append(")");
		return sql;
	}

	/**
	 * INSERT文の中で、挿入する値を構成するブロックを生成する.
	 *
	 * @param metadata エンティティメタ情報
	 * @param mappingColumns マッピングカラム情報
	 * @param sqlConfig SQLコンフィグ
	 * @param ignoreWhenEmpty 空の場合無視するかどうか
	 * @param getParamName パラメータ名取得関数
	 * @return 生成したSQLパーツ
	 */
	protected StringBuilder buildInsertRowBlock(final TableMetadata metadata,
			final Map<String, MappingColumn> mappingColumns,
			final SqlConfig sqlConfig, final boolean ignoreWhenEmpty,
			final Function<TableMetadata.Column, String> getParamName) {
		var sql = new StringBuilder("(").append(System.lineSeparator());
		var firstFlag = true;
		for (var col : metadata.getColumns()) {
			var mappingColumn = mappingColumns.get(col.getCamelColumnName());

			if (!mappingColumns.isEmpty() && mappingColumn == null) {
				// Transient annotation のついているカラムをスキップ
				continue;
			}
			var autoIncrementColumn = mappingColumn != null && mappingColumn.isId()
					&& GenerationType.IDENTITY.equals(mappingColumn.getGeneratedValue().strategy()) ||
					col.isAutoincrement();

			var parts = new StringBuilder().append("\t");
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
				var sequenceName = mappingColumn.getQualifiedSequenceName();
				parts.append(sqlConfig.getDialect().getSequenceNextValSql(sequenceName)).append(System.lineSeparator());
				sql.append(parts);
			} else {
				parts.append("/*").append(getParamName.apply(col)).append("*/''").append(System.lineSeparator());
				if (ignoreWhenEmpty && (col.isNullable() || StringUtils.isNotEmpty(col.getColumnDefault()))
						|| autoIncrementColumn) {
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
	protected String createSqlId(final TableMetadata metadata, final Class<? extends Object> entityType) {
		return "mapping @ " + (entityType != null ? entityType.getSimpleName() : metadata.getTableName());
	}

	/**
	 * 文字列型かどうかを判定する
	 *
	 * @param type JDBC上の型
	 * @return 文字列型の場合<code>true</code>
	 */
	protected boolean isStringType(final int type) {
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
	protected StringBuilder wrapIfComment(final StringBuilder original, final StringBuilder addParts,
			final TableMetadata.Column col) {
		var camelColName = col.getCamelColumnName();
		// フィールドがセットされていない場合はカラム自体を削る
		if (isStringType(col.getDataType()) && emptyStringEqualsNull && !col.isNullable()) {
			original.append("/*IF SF.isNotEmpty(").append(camelColName).append(") */")
					.append(System.lineSeparator());
		} else {
			original.append("/*IF ").append(camelColName).append(" != null */").append(System.lineSeparator());
		}
		original.append(addParts);
		original.append("/*END*/").append(System.lineSeparator());
		return original;
	}

	/**
	 * entityのフィールドの値をExecutionContextにバインドする.
	 *
	 * @param context ExecutionContext
	 * @param entity フィールドをバインドするEntityオブジェクト
	 * @param kind SQL種別
	 * @param getParamName パラメータ名取得関数
	 */
	protected void setFields(final ExecutionContext context, final Object entity, final SqlKind kind,
			final Function<MappingColumn, String> getParamName) {
		var generatedKeyColumns = new ArrayList<String>();
		if (context.getGeneratedKeyColumns() != null) {
			for (var keyColumn : context.getGeneratedKeyColumns()) {
				generatedKeyColumns.add(CaseFormat.CAMEL_CASE.convert(keyColumn));
			}
		}
		for (var column : MappingUtils.getMappingColumns(context.getSchema(), entity.getClass(), kind)) {
			if (SqlKind.INSERT.equals(kind) && generatedKeyColumns.contains(column.getCamelName())) {
				continue;
			}

			context.param(getParamName.apply(column), column.getValue(entity));
		}
	}

	/**
	 * 一括実行時に設定するバインドパラメータ名を取得する.
	 *
	 * @param base 生成の元となるパラメータ名
	 * @param entityIndex エンティティのインデックス
	 * @return 生成したバンドパラメータ名
	 */
	protected String buildBulkParamName(final String base, final int entityIndex) {
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

	/**
	 * TableMetadataのLRUキャッシュをクリアします.
	 */
	public static void clearCache() {
		CACHE.clear();
	}

}
