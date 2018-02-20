package jp.co.future.uroborosql.converter;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.MappingColumn;
import jp.co.future.uroborosql.mapping.MappingUtils;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

		this.columns = MappingUtils.getMappingColumns(entityType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.converter.ResultSetConverter#createRecord(java.sql.ResultSet)
	 */
	@Override
	public E createRecord(final ResultSet rs) throws SQLException {
		if (columnLabels == null) {
			ResultSetMetaData rsmd = rs.getMetaData();
			columnCount = rsmd.getColumnCount();

			// columnLabelsは1始まりの配列で値を格納
			columnLabels = new String[columnCount + 1];
			for (int i = 1; i <= columnCount; i++) {
				columnLabels[i] = rsmd.getColumnLabel(i);
			}
		}

		try {
			E rec = constructor.newInstance();
			for (MappingColumn column : columns) {
				bindValue(rec, rs, column);
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
			if (columnLabels[i].equalsIgnoreCase(column.getName())) {
				column.setValue(rec, mapperManager.getValue(column.getJavaType(), rs, i));
				return;
			}
		}
	}

}
