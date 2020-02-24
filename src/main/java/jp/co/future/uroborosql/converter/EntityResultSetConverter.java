/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.converter;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.MappingColumn;
import jp.co.future.uroborosql.mapping.MappingUtils;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
import jp.co.future.uroborosql.tx.cache.QueryCache;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * 検索結果の1行を任意型に変換する変換器
 *
 * @param <E> エンティティ型
 * @author ota
 *
 */
public class EntityResultSetConverter<E> implements ResultSetConverter<E> {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(EntityResultSetConverter.class);

	private final PropertyMapperManager mapperManager;
	private final Constructor<E> constructor;
	private final MappingColumn[] columns;
	private int columnCount;
	private String[] columnLabels;
	private final QueryCache<E> cache;

	/**
	 * コンストラクタ
	 *
	 * @param entityType エンティティタイプ
	 * @param mapperManager PropertyMapperManager
	 * @param cache QueryCache
	 */
	@SuppressWarnings({ "unchecked" })
	public EntityResultSetConverter(final Class<? extends E> entityType, final PropertyMapperManager mapperManager,
			final Optional<?> cache) {
		this.mapperManager = mapperManager;
		try {
			this.constructor = (Constructor<E>) entityType.getConstructor();
		} catch (NoSuchMethodException e) {
			throw new UroborosqlRuntimeException(e);
		}

		this.columns = MappingUtils.getMappingColumns(entityType);
		if (cache.isPresent()) {
			this.cache = (QueryCache<E>) cache.get();
		} else {
			this.cache = null;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.converter.ResultSetConverter#createRecord(java.sql.ResultSet)
	 */
	@Override
	public E createRecord(final ResultSet rs) throws SQLException {
		try {
			if (columnLabels == null) {
				ResultSetMetaData rsmd = rs.getMetaData();
				columnCount = rsmd.getColumnCount();

				// columnLabelsは1始まりの配列で値を格納
				columnLabels = new String[columnCount + 1];
				for (int i = 1; i <= columnCount; i++) {
					columnLabels[i] = rsmd.getColumnLabel(i);
				}
			}

			E rec = constructor.newInstance();
			for (MappingColumn column : columns) {
				bindValue(rec, rs, column);
			}

			if (this.cache != null && !this.cache.getMetadata().getKeyColumns().isEmpty()) {
				this.cache.put(rec);
			}

			return rec;
		} catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
			LOG.error("Error!!", e);
			throw new UroborosqlRuntimeException(e);
		} catch (SQLException | RuntimeException | Error e) {
			LOG.error("Error!!", e);
			throw e;
		}
	}

	private void bindValue(final E rec, final ResultSet rs, final MappingColumn column) throws SQLException {
		for (int i = 1; i <= columnCount; i++) {
			if (CaseFormat.UPPER_SNAKE_CASE.convert(columnLabels[i]).equalsIgnoreCase(column.getName())) {
				column.setValue(rec, mapperManager.getValue(column.getJavaType(), rs, i));
				return;
			}
		}
	}

}
