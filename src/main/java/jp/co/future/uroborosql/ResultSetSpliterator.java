package jp.co.future.uroborosql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;

import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * ResultSetをStreamに変換するためのSpliterator
 *
 * @author H.Sugimoto
 * @since 0.3.2
 *
 * @param <T>
 */
final class ResultSetSpliterator<T> extends Spliterators.AbstractSpliterator<T> {
	/** ロガー */
	private static final Logger log = LoggerFactory.getLogger(ResultSetSpliterator.class);

	/** 処理対象のResultSet */
	private final ResultSet rs;

	/** ResultSet変換器 */
	private final ResultSetConverter<T> converter;

	/**
	 * コンストラクタ
	 *
	 * @param rs 処理対象のResultSet
	 * @param converter ResultSet変換器
	 */
	ResultSetSpliterator(final ResultSet rs, final ResultSetConverter<T> converter) {
		super(Long.MAX_VALUE, Spliterator.ORDERED | Spliterator.CONCURRENT);
		this.rs = rs;
		this.converter = converter;

		log.debug("ResultSet : {}, Converter : {}", rs, converter);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.util.Spliterator#tryAdvance(java.util.function.Consumer)
	 */
	@Override
	public boolean tryAdvance(final Consumer<? super T> action) {
		try {
			if (!rs.next()) {
				log.debug("tryAdvice : rs : {}, connection : {} , statement : {}, hasNext() : {}", rs, rs
						.getStatement()
						.getConnection(), rs.getStatement(), false);
				rs.close();
				return false;
			}
			T record = converter.createRecord(rs);
			log.debug("tryAdvice : rs : {}, connection : {} , statement : {}, record : {}", rs, rs
					.getStatement()
					.getConnection(), rs.getStatement(), record);
			action.accept(record);
			return true;
		} catch (RuntimeException | Error ex) {
			try {
				if (rs != null && !rs.isClosed()) {
					rs.close();
				}
			} catch (SQLException e) {
				e.printStackTrace();
			}
			throw ex;
		} catch (SQLException ex) {
			try {
				if (rs != null && !rs.isClosed()) {
					rs.close();
				}
			} catch (SQLException e) {
				e.printStackTrace();
			}
			throw new UroborosqlSQLException(ex);
		}
	}
}