package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.NClob;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.sql.Struct;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * JDBCパラメータ生成器
 *
 * @author ota
 */
public final class JdbcParameterFactory {
	/**
	 * コンストラクタ
	 */
	private JdbcParameterFactory() {
	}

	/**
	 * {@link java.sql.Connection#createArrayOf(String, Object[])}のラッパー
	 *
	 * @param conn
	 * @param typeName
	 * @param elements
	 * @return
	 */
	public static Array createArrayOf(final Connection conn, final String typeName, final Object[] elements) {
		try {
			return conn.createArrayOf(typeName, elements);
		} catch (SQLException e) {
			throw new UroborosqlRuntimeException(e);
		}
	}

	/**
	 * {@link java.sql.Connection#createBlob()}のラッパー
	 *
	 * @param conn
	 * @return
	 */
	public static Blob createBlob(final Connection conn) {
		try {
			return conn.createBlob();
		} catch (SQLException e) {
			throw new UroborosqlRuntimeException(e);
		}
	}

	/**
	 * {@link java.sql.Connection#createClob()}のラッパー
	 *
	 * @param conn
	 * @return
	 */
	public static Clob createClob(final Connection conn) {
		try {
			return conn.createClob();
		} catch (SQLException e) {
			throw new UroborosqlRuntimeException(e);
		}
	}

	/**
	 * {@link java.sql.Connection#createNClob()}のラッパー
	 *
	 * @param conn
	 * @return
	 */
	public static NClob createNClob(final Connection conn) {
		try {
			return conn.createNClob();
		} catch (SQLException e) {
			throw new UroborosqlRuntimeException(e);
		}
	}

	/**
	 * {@link java.sql.Connection#createSQLXML()}のラッパー
	 *
	 * @param conn
	 * @return
	 */
	public static SQLXML createSQLXML(final Connection conn) {
		try {
			return conn.createSQLXML();
		} catch (SQLException e) {
			throw new UroborosqlRuntimeException(e);
		}
	}

	/**
	 * {@link java.sql.Connection#createStruct(String, Object[])}のラッパー
	 *
	 * @param conn
	 * @param typeName
	 * @param attributes
	 * @return
	 */
	public static Struct createStruct(final Connection conn, final String typeName, final Object[] attributes) {
		try {
			return conn.createStruct(typeName, attributes);
		} catch (SQLException e) {
			throw new UroborosqlRuntimeException(e);
		}
	}
}
