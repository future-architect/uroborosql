package jp.co.future.uroborosql.parameter;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLType;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

/**
 * 出力パラメータオブジェクト。<br>
 * ストアドプロシージャの出力パラメータ情報を保持する。<br>
 *
 * @author H.Sugimoto
 */
public class OutParameter extends Parameter {
	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param sqlType {@link java.sql.SQLType}で指定される型
	 */
	public OutParameter(final String parameterName, final SQLType sqlType) {
		super(parameterName, null, sqlType);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param sqlType {@link java.sql.Types}で指定される型
	 */
	public OutParameter(final String parameterName, final int sqlType) {
		super(parameterName, null, sqlType);
	}

	/**
	 * InOutParameter用コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.SQLType}で指定される型
	 */
	OutParameter(final String parameterName, final Object value, final SQLType sqlType) {
		super(parameterName, value, sqlType);
	}

	/**
	 * InOutParameter用コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types}で指定される型
	 */
	OutParameter(final String parameterName, final Object value, final int sqlType) {
		super(parameterName, value, sqlType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.Parameter#setParameter(java.sql.PreparedStatement, int, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public int setParameter(final PreparedStatement preparedStatement, final int index,
			final BindParameterMapperManager parameterMapperManager) throws SQLException {
		return setOutParameter((CallableStatement) preparedStatement, index);
	}

	/**
	 * ステートメントに出力パラメータを登録。
	 * @param callableStatement コーラブルステートメント
	 * @param index パラメータインデックス
	 * @return 次のパラメータインデックス
	 * @throws SQLException SQL例外
	 */
	protected int setOutParameter(final CallableStatement callableStatement, int index) throws SQLException {
		callableStatement.registerOutParameter(index, sqlType);
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
		return "パラメータ名[" + parameterName + "], SQL型[" + sqlType + "]";
	}
}
