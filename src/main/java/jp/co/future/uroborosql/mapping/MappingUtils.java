package jp.co.future.uroborosql.mapping;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.LinkedHashMap;
import java.util.Map;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.annotations.Column;
import jp.co.future.uroborosql.mapping.annotations.Transient;
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
	}

	private static final Map<Class<?>, MappingColumn[]> CHACHE = new LinkedHashMap<Class<?>, MappingColumn[]>() {
		@Override
		protected boolean removeEldestEntry(final Map.Entry<Class<?>, MappingColumn[]> eldest) {
			return size() > 10;
		}
	};

	/**
	 * エンティティ型からテーブル情報の取得
	 *
	 * @param entityType エンティティ型
	 * @return テーブル情報
	 */
	public static Table geTable(final Class<?> entityType) {
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
		MappingColumn[] cols;
		synchronized (CHACHE) {
			cols = CHACHE.get(entityType);
		}
		if (cols != null) {
			return cols;
		}

		Map<String, MappingColumn> fieldsMap = new LinkedHashMap<>();
		JavaType.ImplementClass implementClass = new JavaType.ImplementClass(entityType);
		walkFields(entityType, implementClass, fieldsMap);

		cols = fieldsMap.values().toArray(new MappingColumn[fieldsMap.size()]);
		synchronized (CHACHE) {
			CHACHE.put(entityType, cols);
		}
		return cols;
	}

	private static void walkFields(final Class<?> type, final JavaType.ImplementClass implementClass, final Map<String, MappingColumn> fieldsMap) {
		if (type.equals(Object.class)) {
			return;
		}
		Class<?> superclass = type.getSuperclass();
		walkFields(superclass, implementClass, fieldsMap);

		for (Field field : type.getDeclaredFields()) {
			if (Modifier.isStatic(field.getModifiers()) || Modifier.isFinal(field.getModifiers())) {
				continue;
			}
			if (field.getAnnotation(Transient.class) != null) {
				continue;// 除外
			}
			JavaType javaType = JavaType.of(implementClass, field);
			MappingColumn mappingColumn = new MappingColumnImpl(field, javaType);

			fieldsMap.put(field.getName(), mappingColumn);
			field.setAccessible(true);
		}
	}
}
