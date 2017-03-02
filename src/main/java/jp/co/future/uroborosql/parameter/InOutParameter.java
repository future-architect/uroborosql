package jp.co.future.uroborosql.parameter;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLType;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

/**
 * 入出力パラメータオブジェクト。<br>
 * ストアドプロシージャの入出力パラメータ情報を保持する。
 *
 * @author H.Sugimoto
 */
public class InOutParameter extends OutParameter {

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.SQLType}で指定される型
	 */
	public InOutParameter(final String parameterName, final Object value, final SQLType sqlType) {
		super(parameterName, value, sqlType);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types}で指定される型
	 */
	public InOutParameter(final String parameterName, final Object value, final int sqlType) {
		super(parameterName, value, sqlType);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parameter.OutParameter#setParameter(java.sql.PreparedStatement, int, jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager)
	 */
	@Override
	public int setParameter(final PreparedStatement preparedStatement, final int index,
			final BindParameterMapperManager parameterMapperManager) throws SQLException {
		setInParameter(preparedStatement, index, parameterMapperManager);
		return setOutParameter((CallableStatement) preparedStatement, index);
	}
}
