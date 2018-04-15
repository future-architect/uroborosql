/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter;

import java.io.InputStream;
import java.sql.JDBCType;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

/**
 * ストリームパラメータオブジェクト。<br>
 * ストリームパラメータ情報を保持する。<br>
 *
 * @author H.Sugimoto
 */
public class StreamParameter extends Parameter {

	/**
	 * ストリーム
	 */
	protected InputStream stream;

	/**
	 * ストリーム長
	 */
	protected int len = -1;

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param stream ストリーム
	 */
	public StreamParameter(final String parameterName, final InputStream stream) {
		this(parameterName, stream, -1);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param stream ストリーム
	 * @param len ストリーム長
	 */
	public StreamParameter(final String parameterName, final InputStream stream, final int len) {
		super(parameterName, "[BLOB]", JDBCType.BLOB);
		this.stream = stream;
		this.len = len;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.Parameter#setParameter(java.sql.PreparedStatement, int, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public int setParameter(final PreparedStatement preparedStatement, final int index,
			final BindParameterMapperManager parameterMapperManager) throws SQLException {
		return setStreamParameter(preparedStatement, index, parameterMapperManager);
	}

	/**
	 * ステートメントにストリームパラメータを登録。
	 *
	 * @param preparedStatement ステートメント
	 * @param index パラメータインデックス
	 * @param parameterMapperManager パラメータ変換管理クラス
	 * @return 次のパラメータインデックス
	 * @throws SQLException SQL例外
	 */
	protected int setStreamParameter(final PreparedStatement preparedStatement, int index,
			final BindParameterMapperManager parameterMapperManager) throws SQLException {

		if (len > -1) {
			preparedStatement.setBinaryStream(index, stream, len);
		} else {
			preparedStatement.setBinaryStream(index, stream);
		}

		parameterLog(index);
		index++;

		return index;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.Parameter#toString()
	 */
	@Override
	public String toString() {
		return "Parameter name[" + parameterName + "], Value[" + stream.toString() + "], SQL type[" + sqlType + "]";
	}
}
