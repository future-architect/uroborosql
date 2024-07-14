/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
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
	 * @param conn コネクション
	 * @param typeName 配列の要素がマッピングされる型のSQL名。typeNameはデータベース固有の名前で、組込み型、ユーザー定義型、またはこのデータベースでサポートされる標準SQL型の名前のこと。これは、Array.getBaseTypeNameで返される値
	 * @param elements 返されるオブジェクトを生成する要素
	 * @return 指定されたSQL型に要素がマッピングされるArrayオブジェクト
	 *
	 * @see java.sql.Connection#createArrayOf(String, Object[])
	 */
	public static Array createArrayOf(final Connection conn, final String typeName, final Object[] elements) {
		try {
			return conn.createArrayOf(typeName, elements);
		} catch (SQLException ex) {
			throw new UroborosqlRuntimeException(ex);
		}
	}

	/**
	 * {@link java.sql.Connection#createBlob()}のラッパー
	 *
	 * @param conn コネクション
	 * @return Blobインタフェースを実装しているオブジェクト
	 *
	 * @see java.sql.Connection#createBlob()
	 */
	public static Blob createBlob(final Connection conn) {
		try {
			return conn.createBlob();
		} catch (SQLException ex) {
			throw new UroborosqlRuntimeException(ex);
		}
	}

	/**
	 * {@link java.sql.Connection#createClob()}のラッパー
	 *
	 * @param conn コネクション
	 * @return Clobインタフェースを実装しているオブジェクト
	 *
	 * @see java.sql.Connection#createClob()
	 */
	public static Clob createClob(final Connection conn) {
		try {
			return conn.createClob();
		} catch (SQLException ex) {
			throw new UroborosqlRuntimeException(ex);
		}
	}

	/**
	 * {@link java.sql.Connection#createNClob()}のラッパー
	 *
	 * @param conn コネクション
	 * @return NClobインタフェースを実装しているオブジェクト
	 *
	 * @see java.sql.Connection#createNClob()
	 */
	public static NClob createNClob(final Connection conn) {
		try {
			return conn.createNClob();
		} catch (SQLException ex) {
			throw new UroborosqlRuntimeException(ex);
		}
	}

	/**
	 * {@link java.sql.Connection#createSQLXML()}のラッパー
	 *
	 * @param conn コネクション
	 * @return SQLXMLインタフェースを実装しているオブジェクト
	 *
	 * @see java.sql.Connection#createSQLXML()
	 */
	public static SQLXML createSQLXML(final Connection conn) {
		try {
			return conn.createSQLXML();
		} catch (SQLException ex) {
			throw new UroborosqlRuntimeException(ex);
		}
	}

	/**
	 * {@link java.sql.Connection#createStruct(String, Object[])}のラッパー
	 *
	 * @param conn コネクション
	 * @param typeName このStructオブジェクトがマッピングされるSQL構造化型のSQL型名。typeNameは、このデータベースに定義されたユーザー定義型の名前。これは、Struct.getSQLTypeNameで返される値。
	 * @param attributes 返されるオブジェクトを生成する属性
	 * @return 指定されたSQL型にマッピングされ、かつ指定された属性で生成されるStructオブジェクト
	 *
	 * @see java.sql.Connection#createStruct(String, Object[])
	 */
	public static Struct createStruct(final Connection conn, final String typeName, final Object[] attributes) {
		try {
			return conn.createStruct(typeName, attributes);
		} catch (SQLException ex) {
			throw new UroborosqlRuntimeException(ex);
		}
	}
}
