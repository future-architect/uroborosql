/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.annotations.Column;
import jp.co.future.uroborosql.mapping.annotations.GeneratedValue;
import jp.co.future.uroborosql.mapping.annotations.Id;
import jp.co.future.uroborosql.mapping.annotations.SequenceGenerator;
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
		private final boolean isId;
		private final GeneratedValue generatedValue;
		private final SequenceGenerator sequenceGenerator;
		private final Transient transientAnno;
		private final Version versionAnno;

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
			this.isId = field.getAnnotation(Id.class) != null;
			this.generatedValue = field.getAnnotation(GeneratedValue.class);
			this.sequenceGenerator = field.getAnnotation(SequenceGenerator.class);
			this.transientAnno = field.getAnnotation(Transient.class);
			this.versionAnno = field.getAnnotation(Version.class);

			if (this.isId && this.generatedValue == null) {
				throw new UroborosqlRuntimeException("@Id annotation is set in the field [" + field.getName()
						+ "]. However, @GeneratedValue annotation is not set.");
			}
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#getValue(java.lang.Object)
		 */
		@Override
		public Object getValue(final Object entity) {
			try {
				return this.field.get(entity);
			} catch (IllegalArgumentException | IllegalAccessException e) {
				throw new UroborosqlRuntimeException(e);
			}
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#setValue(java.lang.Object, java.lang.Object)
		 */
		@Override
		public void setValue(final Object entity, final Object value) {
			try {
				this.field.set(entity, value);
			} catch (IllegalArgumentException | IllegalAccessException e) {
				throw new UroborosqlRuntimeException(e);
			}
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#getJavaType()
		 */
		@Override
		public JavaType getJavaType() {
			return this.javaType;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#getName()
		 */
		@Override
		public String getName() {
			return this.name;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#getCamelName()
		 */
		@Override
		public String getCamelName() {
			return this.camelName;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#isId()
		 */
		@Override
		public boolean isId() {
			return this.isId;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#getGeneratedValue()
		 */
		@Override
		public GeneratedValue getGeneratedValue() {
			return this.generatedValue;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#getSequenceGenerator()
		 */
		@Override
		public SequenceGenerator getSequenceGenerator() {
			return this.sequenceGenerator;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#getTransient()
		 */
		@Override
		public Transient getTransient() {
			return this.transientAnno;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#isTransient(jp.co.future.uroborosql.enums.SqlKind)
		 */
		@Override
		public boolean isTransient(final SqlKind sqlKind) {
			if (this.transientAnno == null) {
				return false;
			}

			switch (sqlKind) {
			case INSERT:
				return this.transientAnno.insert();
			case UPDATE:
				return this.transientAnno.update();
			default:
				return this.transientAnno.insert() && this.transientAnno.update();
			}
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#isVersion()
		 */
		@Override
		public boolean isVersion() {
			return this.versionAnno != null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.MappingColumn#getVersion()
		 */
		@Override
		public Version getVersion() {
			return this.versionAnno;
		}
	}

	private static final Map<Class<?>, Map<SqlKind, MappingColumn[]>> CACHE = new LinkedHashMap<Class<?>, Map<SqlKind, MappingColumn[]>>() {
		private final int cacheSize = Integer.valueOf(System.getProperty("uroborosql.entity.cache.size", "30"));

		@Override
		protected boolean removeEldestEntry(final Map.Entry<Class<?>, Map<SqlKind, MappingColumn[]>> eldest) {
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
	 * @param camelColumnName 取得するカラムのキャメルケース名
	 * @return カラムマッピング情報
	 * @exception UroborosqlRuntimeException 指定したキャメルケースカラム名に該当する{@link MappingColumn}が見つからなかった場合
	 */
	public static MappingColumn getMappingColumn(final Class<?> entityType, final String camelColumnName) {
		return getMappingColumn(entityType, SqlKind.NONE, camelColumnName);
	}

	/**
	 * カラムマッピング情報取得
	 *
	 * @param entityType エンティティ型
	 * @param kind SQL種別
	 * @param camelColumnName 取得するカラムのキャメルケース名
	 * @return カラムマッピング情報
	 * @exception UroborosqlRuntimeException 指定したキャメルケースカラム名に該当する{@link MappingColumn}が見つからなかった場合
	 */
	public static MappingColumn getMappingColumn(final Class<?> entityType, final SqlKind kind,
			final String camelColumnName) {
		return getMappingColumnMap(entityType, kind).entrySet().stream()
				.filter(entry -> entry.getKey().equals(camelColumnName)).map(entry -> entry.getValue()).findFirst()
				.orElseThrow(() -> new UroborosqlRuntimeException("No such column found. col:" + camelColumnName));
	}

	/**
	 * カラムマッピング情報取得
	 *
	 * @param entityType エンティティ型
	 * @return カラムマッピング情報
	 */
	public static MappingColumn[] getMappingColumns(final Class<?> entityType) {
		return getMappingColumns(entityType, SqlKind.NONE);
	}

	/**
	 * カラムマッピング情報取得
	 *
	 * @param entityType エンティティ型
	 * @param kind SQL種別
	 * @return カラムマッピング情報
	 */
	public static MappingColumn[] getMappingColumns(final Class<?> entityType, final SqlKind kind) {
		if (entityType == null) {
			return new MappingColumn[0];
		}

		Map<SqlKind, MappingColumn[]> cols;
		synchronized (CACHE) {
			cols = CACHE.get(entityType);
		}
		if (cols != null) {
			return cols.computeIfAbsent(kind, k -> cols.get(SqlKind.NONE));
		}

		Map<SqlKind, Map<String, MappingColumn>> fieldsMap = Stream.of(SqlKind.NONE, SqlKind.INSERT, SqlKind.UPDATE)
				.collect(Collectors.toMap(e -> e, e -> new LinkedHashMap<>()));

		JavaType.ImplementClass implementClass = new JavaType.ImplementClass(entityType);

		walkFields(entityType, implementClass, fieldsMap);

		final Map<SqlKind, MappingColumn[]> entityCols = fieldsMap.entrySet().stream()
				.collect(Collectors.toConcurrentMap(e -> e.getKey(),
						e -> e.getValue().values().toArray(new MappingColumn[e.getValue().size()])));

		synchronized (CACHE) {
			CACHE.put(entityType, entityCols);
		}
		return entityCols.computeIfAbsent(kind, k -> entityCols.get(SqlKind.NONE));
	}

	/**
	 * カラム名（小文字）をキーとしたMapにカラムマッピング情報を取得
	 *
	 * @param entityType エンティティ型
	 * @param kind SQL種別
	 * @return カラムマッピング情報
	 */
	public static Map<String, MappingColumn> getMappingColumnMap(final Class<?> entityType, final SqlKind kind) {
		return Arrays.stream(MappingUtils.getMappingColumns(entityType, kind))
				.collect(Collectors.toMap(MappingColumn::getCamelName, c -> c));
	}

	/**
	 * IDカラムマッピング情報を返す
	 *
	 * @param entityType エンティティ型
	 * @return カラムマッピング情報
	 */
	public static MappingColumn[] getIdMappingColumns(final Class<?> entityType) {
		return Arrays.stream(MappingUtils.getMappingColumns(entityType)).filter(c -> c.isId())
				.toArray(MappingColumn[]::new);
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
			final Map<SqlKind, Map<String, MappingColumn>> fieldsMap) {
		if (type.equals(Object.class)) {
			return;
		}
		Class<?> superclass = type.getSuperclass();
		walkFields(superclass, implementClass, fieldsMap);

		Map<String, MappingColumn> noneColumns = fieldsMap.get(SqlKind.NONE);
		Map<String, MappingColumn> insertColumns = fieldsMap.get(SqlKind.INSERT);
		Map<String, MappingColumn> updateColumns = fieldsMap.get(SqlKind.UPDATE);

		for (Field field : type.getDeclaredFields()) {
			if (Modifier.isStatic(field.getModifiers()) || Modifier.isFinal(field.getModifiers())) {
				continue;
			}
			JavaType javaType = JavaType.of(implementClass, field);
			MappingColumn mappingColumn = new MappingColumnImpl(field, javaType);

			String fieldName = field.getName();
			noneColumns.put(fieldName, mappingColumn);

			if (mappingColumn.isTransient(SqlKind.NONE)) {
				continue;// 除外
			}
			if (!mappingColumn.isTransient(SqlKind.INSERT)) {
				insertColumns.put(fieldName, mappingColumn);
			}
			if (!mappingColumn.isTransient(SqlKind.UPDATE)) {
				updateColumns.put(fieldName, mappingColumn);
			}
		}
	}
}
