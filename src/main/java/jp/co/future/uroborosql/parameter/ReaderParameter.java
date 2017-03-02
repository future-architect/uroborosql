package jp.co.future.uroborosql.parameter;

import java.io.Reader;
import java.sql.JDBCType;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

/**
 * ストリームパラメータオブジェクト。<br>
 * ストリームパラメータ情報を保持する。<br>
 *
 * @author miyahara
 */
public class ReaderParameter extends Parameter {

	/**
	 * ストリーム
	 */
	protected Reader reader;

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
	public ReaderParameter(final String parameterName, final Reader reader) {
		this(parameterName, reader, -1);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param stream ストリーム
	 * @param len ストリーム長
	 */
	public ReaderParameter(final String parameterName, final Reader reader, final int len) {
		super(parameterName, "[CLOB]", JDBCType.CLOB);
		this.reader = reader;
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
		return setReaderParameter(preparedStatement, index);
	}

	/**
	 * ステートメントにストリームパラメータを登録。
	 *
	 * @param preparedStatement ステートメント
	 * @param index パラメータインデックス
	 * @return 次のパラメータインデックス
	 * @throws SQLException SQL例外
	 */
	protected int setReaderParameter(final PreparedStatement preparedStatement, int index) throws SQLException {
		if (len > -1) {
			preparedStatement.setCharacterStream(index, reader, len);
		} else {
			preparedStatement.setCharacterStream(index, reader);
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
		return "パラメータ名[" + parameterName + "], 値[" + reader.toString() + "], SQL型[" + sqlType + "]";
	}

}
