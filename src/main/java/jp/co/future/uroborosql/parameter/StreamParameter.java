package jp.co.future.uroborosql.parameter;

import java.io.InputStream;
import java.sql.JDBCType;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLType;
import java.util.Objects;

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
	 * @param sqlType {@link java.sql.SQLType}で表される型
	 */
	public StreamParameter(final String parameterName, final InputStream stream, final SQLType sqlType) {
		this(parameterName, stream, -1, sqlType);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param stream ストリーム
	 * @param sqlType {@link java.sql.Types}で表される型
	 */
	public StreamParameter(final String parameterName, final InputStream stream, final int sqlType) {
		this(parameterName, stream, -1, sqlType);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param stream ストリーム
	 * @param len ストリーム長
	 * @param sqlType {@link java.sql.SQLType}で表される型
	 */
	public StreamParameter(final String parameterName, final InputStream stream, final int len, final SQLType sqlType) {
		super(parameterName, "[BLOB]", sqlType);
		this.stream = stream;
		this.len = len;
	}

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param stream ストリーム
	 * @param len ストリーム長
	 * @param sqlType {@link java.sql.Types}で表される型
	 */
	public StreamParameter(final String parameterName, final InputStream stream, final int len, final int sqlType) {
		super(parameterName, "[BLOB]", sqlType);
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

		if (Objects.equals(sqlType, JDBCType.BLOB)) {
			if (len > -1) {
				preparedStatement.setBinaryStream(index, stream, len);
			} else {
				preparedStatement.setBinaryStream(index, stream);
			}
		} else if (Objects.equals(sqlType, JDBCType.CLOB)) {
			if (len > -1) {
				preparedStatement.setAsciiStream(index, stream, len);
			} else {
				preparedStatement.setAsciiStream(index, stream);
			}
		} else {
			return super.setParameter(preparedStatement, index, parameterMapperManager);
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
		return "パラメータ名[" + parameterName + "], 値[" + stream.toString() + "], SQL型[" + sqlType + "]";
	}
}
