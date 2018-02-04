package jp.co.future.uroborosql.mapping;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.annotations.Column;
import jp.co.future.uroborosql.mapping.annotations.Transient;
import jp.co.future.uroborosql.mapping.annotations.Version;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * マッピング情報ユーティリティ
 *
 * @author ota
 */
public final class MappingUtils {
	private MappingUtils() {
	}

	private static class TableImpl implements Table {
		private final String name;
		private final String schema;

		protected TableImpl(final String name, final String schema) {
			this.name = name;
			this.schema = schema;
		}

		@Override
		public String getName() {
			return this.name;
		}

		@Override
		public String getSchema() {
			return this.schema;
		}
	}

	private static class MappingColumnImpl implements MappingColumn {
		private final Field field;
		private final JavaType javaType;
		private final String name;
		private final String camelName;
		private final boolean isVersion;

		MappingColumnImpl(final Field field, final JavaType javaType) {
			this.field = field;
			this.javaType = javaType;
			Column column = field.getAnnotation(Column.class);
			// アクセス可能にする
			field.setAccessible(true);

			if (column != null) {
				this.name = column.name();
				this.camelName = CaseFormat.CAMEL_CASE.convert(column.name());
			} else {
				this.name = CaseFormat.UPPER_SNAKE_CASE.convert(field.getName());
				this.camelName = field.getName();
			}
			this.isVersion = field.getAnnotation(Version.class) != null;
		}

		@Override
		public Object getValue(final Object entity) {
			try {
				return this.field.get(entity);
			} catch (IllegalArgumentException | IllegalAccessException e) {
				throw new UroborosqlRuntimeException(e);
			}
		}

		@Override
		public void setValue(final Object entity, final Object value) {
			try {
				this.field.set(entity, value);
			} catch (IllegalArgumentException | IllegalAccessException e) {
				throw new UroborosqlRuntimeException(e);
			}
		}

		@Override
		public JavaType getJavaType() {
			return this.javaType;
		}

		@Override
		public String getName() {
			return this.name;
		}

		@Override
		public String getCamelName() {
			return this.camelName;
		}

		@Override
		public boolean isVersion() {
			return isVersion;
		}
	}

	private static final Map<Class<?>, Map<SqlStatement, MappingColumn[]>> CACHE = new LinkedHashMap<Class<?>, Map<SqlStatement, MappingColumn[]>>() {
		private final int cacheSize = Integer.valueOf(System.getProperty("uroborosql.entity.cache.size", "30"));

		@Override
		protected boolean removeEldestEntry(final Map.Entry<Class<?>, Map<SqlStatement, MappingColumn[]>> eldest) {
			return size() > cacheSize;
		}
	};

	/**
	 * エンティティ型からテーブル情報の取得
	 *
	 * @param entityType エンティティ型
	 * @return テーブル情報
	 */
	public static Table getTable(final Class<?> entityType) {
		jp.co.future.uroborosql.mapping.annotations.Table table = entityType.getAnnotation(
				jp.co.future.uroborosql.mapping.annotations.Table.class);
		if (table != null) {
			return new TableImpl(table.name(), table.schema());
		}
		String baseName = entityType.getSimpleName();
		if (baseName.endsWith("Entity")) {
			baseName = baseName.substring(0, baseName.length() - 6);
		}
		return new TableImpl(CaseFormat.UPPER_SNAKE_CASE.convert(baseName), "");
	}

	/**
	 * カラムマッピング情報取得
	 *
	 * @param entityType エンティティ型
	 * @return カラムマッピング情報
	 */
	public static MappingColumn[] getMappingColumns(final Class<?> entityType) {
		return getMappingColumns(entityType, SqlStatement.NONE);
	}

	/**
	 * カラムマッピング情報取得
	 *
	 * @param entityType エンティティ型
	 * @param stmt SQLステートメントタイプ
	 * @return カラムマッピング情報
	 */
	public static MappingColumn[] getMappingColumns(final Class<?> entityType, final SqlStatement stmt) {
		Map<SqlStatement, MappingColumn[]> cols;
		synchronized (CACHE) {
			cols = CACHE.get(entityType);
		}
		if (cols != null) {
			return cols.computeIfAbsent(stmt, k -> cols.get(SqlStatement.NONE));
		}

		Map<SqlStatement, Map<String, MappingColumn>> fieldsMap =
				Stream.of(SqlStatement.NONE, SqlStatement.INSERT, SqlStatement.UPDATE).collect(
						Collectors.toMap(e -> e, e -> new LinkedHashMap<String, MappingColumn>()));

		JavaType.ImplementClass implementClass = new JavaType.ImplementClass(entityType);

		walkFields(entityType, implementClass, fieldsMap);

		final Map<SqlStatement, MappingColumn[]> entityCols = fieldsMap
				.entrySet()
				.stream()
				.collect(
						Collectors.toConcurrentMap(e -> e.getKey(),
								e -> e.getValue().values().toArray(new MappingColumn[e.getValue().size()])));

		synchronized (CACHE) {
			CACHE.put(entityType, entityCols);
		}
		return entityCols.computeIfAbsent(stmt, k -> entityCols.get(SqlStatement.NONE));
	}

	/**
	 * バージョン情報のカラムマッピング情報を返す
	 *
	 * @param entityType エンティティ型
	 * @return カラムマッピング情報
	 */
	public static Optional<MappingColumn> getVersionMappingColumn(final Class<?> entityType) {
		return Arrays.stream(getMappingColumns(entityType))
				.filter(MappingColumn::isVersion)
				.findFirst();
	}

	private static void walkFields(final Class<?> type, final JavaType.ImplementClass implementClass,
			final Map<SqlStatement, Map<String, MappingColumn>> fieldsMap) {
		if (type.equals(Object.class)) {
			return;
		}
		Class<?> superclass = type.getSuperclass();
		walkFields(superclass, implementClass, fieldsMap);

		Map<String, MappingColumn> noneColumns = fieldsMap.get(SqlStatement.NONE);
		Map<String, MappingColumn> insertColumns = fieldsMap.get(SqlStatement.INSERT);
		Map<String, MappingColumn> updateColumns = fieldsMap.get(SqlStatement.UPDATE);

		for (Field field : type.getDeclaredFields()) {
			if (Modifier.isStatic(field.getModifiers()) || Modifier.isFinal(field.getModifiers())) {
				continue;
			}
			Transient t = field.getAnnotation(Transient.class);
			if (t != null && t.insert() && t.update()) {
				continue;// 除外
			}
			JavaType javaType = JavaType.of(implementClass, field);
			MappingColumn mappingColumn = new MappingColumnImpl(field, javaType);

			String fieldName = field.getName();
			noneColumns.put(fieldName, mappingColumn);
			if (t == null || (t != null && !t.insert())) {
				insertColumns.put(fieldName, mappingColumn);
			}
			if (t == null || (t != null && !t.update())) {
				updateColumns.put(fieldName, mappingColumn);
			}

			field.setAccessible(true);
		}
	}
}
