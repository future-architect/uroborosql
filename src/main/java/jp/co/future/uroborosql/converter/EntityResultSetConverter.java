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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.MappingColumn;
import jp.co.future.uroborosql.mapping.MappingUtils;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
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
	private final Map<String, MappingColumn> mappingColumnMap;
	private Map<MappingColumn, Integer> columnPositionMap;

	/**
	 * コンストラクタ
	 *
	 * @param entityType エンティティタイプ
	 * @param mapperManager PropertyMapperManager
	 */
	@SuppressWarnings({ "unchecked" })
	public EntityResultSetConverter(final Class<? extends E> entityType, final PropertyMapperManager mapperManager) {
		this.mapperManager = mapperManager;
		try {
			this.constructor = (Constructor<E>) entityType.getConstructor();
		} catch (NoSuchMethodException e) {
			throw new UroborosqlRuntimeException(e);
		}

		this.mappingColumnMap = Arrays.stream(MappingUtils.getMappingColumns(entityType))
				.collect(Collectors.toMap(c -> CaseFormat.UPPER_SNAKE_CASE.convert(c.getName()), Function.identity()));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.converter.ResultSetConverter#createRecord(java.sql.ResultSet)
	 */
	@Override
	public E createRecord(final ResultSet rs) throws SQLException {
		try {
			if (columnPositionMap == null) {
				ResultSetMetaData rsmd = rs.getMetaData();
				int columnCount = rsmd.getColumnCount();

				columnPositionMap = new HashMap<>(columnCount);
				// columnLabelsは1始まりの配列で値を格納
				for (int i = 1; i <= columnCount; i++) {
					String columnLabel = CaseFormat.UPPER_SNAKE_CASE.convert(rsmd.getColumnLabel(i));
					MappingColumn col = mappingColumnMap.get(columnLabel);
					if (col != null) {
						columnPositionMap.put(col, i);
					}
				}
			}

			E rec = constructor.newInstance();
			for (Map.Entry<MappingColumn, Integer> entry : columnPositionMap.entrySet()) {
				MappingColumn column = entry.getKey();
				int position = entry.getValue();
				column.setValue(rec, mapperManager.getValue(column.getJavaType(), rs, position));
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
}
