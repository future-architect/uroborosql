package jp.co.future.uroborosql.parameter;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.sql.JDBCType;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLType;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

/**
 * パラメータオブジェクト。<br>
 * SQLへバインドするパラメータを保持する。<br>
 *
 * @author H.Sugimoto
 */
public class Parameter {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(Parameter.class);

	/** 未設定のSQLType */
	protected static final SQLType SQL_TYPE_NOT_SET = null;

	/**
	 * パラメータ名
	 */
	protected final String parameterName;

	/**
	 * パラメータ値
	 */
	protected final Object value;

	/**
	 * SQL型
	 */
	protected final SQLType sqlType;

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param value 値
	 */
	public Parameter(final String parameterName, final Object value) {
		this(parameterName, value, SQL_TYPE_NOT_SET);
	}

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.SQLType} で表される型
	 */
	public Parameter(final String parameterName, final Object value, final SQLType sqlType) {
		this.parameterName = parameterName;
		this.value = value;
		this.sqlType = sqlType;
	}

	/**
	 * コンストラクタ。
	 *
	 * @param parameterName パラメータ名
	 * @param value 値
	 * @param sqlType {@link java.sql.Types} で表される型
	 */
	public Parameter(final String parameterName, final Object value, final int sqlType) {
		this.parameterName = parameterName;
		this.value = value;
		this.sqlType = toSqlType(sqlType);
	}

	/**
	 * サブパラメータを生成する。 パラメータ値がBeanの場合、プロパティ名に対応するフィールド値をパラメータ値とする サブパラメータを作成して返す。
	 *
	 * @param propertyName プロパティ名
	 * @return パラメータ
	 */
	@SuppressWarnings("rawtypes")
	public Parameter createSubParameter(final String propertyName) {
		String subParameterName = parameterName + "." + propertyName;
		Object subValue = null;
		if (value != null) {
			if (value instanceof Map) {
				subValue = ((Map) value).get(propertyName);
				if (subValue == null) {
					LOG.warn("プロパティにアクセスできないためサブパラメータ値にNULLを設定します。[{}]", subParameterName);
				}
			} else {
				try {
					Field field = value.getClass().getDeclaredField(propertyName);
					field.setAccessible(true);
					subValue = field.get(value);
				} catch (Exception e) {
					LOG.warn("プロパティにアクセスできないためサブパラメータ値にNULLを設定します。[{}]", subParameterName, e);
				}
			}
		}

		return new Parameter(subParameterName, subValue);
	}

	/**
	 * ステートメントへパラメータ値をバインド。
	 *
	 * @param preparedStatement ステートメント
	 * @param index パラメータインデックス
	 * @param parameterMapperManager パラメータ変換管理クラス
	 * @return 次のパラメータインデックス
	 * @throws SQLException SQL例外
	 */
	public int setParameter(final PreparedStatement preparedStatement, final int index,
			final BindParameterMapperManager parameterMapperManager) throws SQLException {
		return setInParameter(preparedStatement, index, parameterMapperManager);
	}

	/**
	 * ステートメントへ入力パラメータ値をバインド。
	 *
	 * @param preparedStatement ステートメント
	 * @param index パラメータインデックス
	 * @param parameterMapperManager パラメータ変換管理クラス
	 * @return 次のパラメータインデックス
	 * @throws SQLException SQL例外
	 */
	@SuppressWarnings("rawtypes")
	protected int setInParameter(final PreparedStatement preparedStatement, final int index,
			final BindParameterMapperManager parameterMapperManager) throws SQLException {
		int parameterIndex = index;
		if (value instanceof List) {
			for (Object e : (List) value) {
				setParameterObject(preparedStatement, parameterIndex, e, parameterMapperManager);

				parameterLog(parameterIndex);
				parameterIndex++;
			}
		} else if (value != null && value.getClass().isArray() && !parameterMapperManager.existsArrayMapper(value)//マッピング対象のArrayはここで処理しない
		) {
			int length = Array.getLength(value);
			for (int i = 0; i < length; i++) {
				setParameterObject(preparedStatement, parameterIndex, Array.get(value, i), parameterMapperManager);

				parameterLog(parameterIndex);
				parameterIndex++;
			}
		} else {
			setParameterObject(preparedStatement, parameterIndex, value, parameterMapperManager);

			parameterLog(parameterIndex);
			parameterIndex++;
		}

		return parameterIndex;
	}

	/**
	 * パラメータ設定ログ出力。
	 *
	 * @param index パラメータインデックス
	 */
	protected void parameterLog(final int index) {
		if (LOG.isDebugEnabled() && Boolean.FALSE.toString().equals(MDC.get("SuppressParameterLogOutput"))) {
			LOG.debug("パラメータを設定します。[INDEX[{}], {}]", index, this);
		}
	}

	/**
	 * パラメータ名取得。
	 *
	 * @return パラメータ名
	 */
	public String getParameterName() {
		return parameterName;
	}

	/**
	 * パラメータ値取得。
	 *
	 * @return パラメータ値
	 */
	public Object getValue() {
		return value;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "パラメータ名[" + parameterName + "], 値[" + value + "], 型["
				+ (value == null ? "NULL" : value.getClass().getSimpleName())
				+ (Objects.equals(sqlType, SQL_TYPE_NOT_SET) ? "]" : "], SQL型[" + sqlType.getName() + "]");
	}

	/**
	 * {@link java.sql.Types} の値を {@link java.sql.SQLType} に変換する
	 * @param sqlType {@link java.sql.Types} の値
	 * @return {@link java.sql.SQLType} の値
	 */
	private SQLType toSqlType(final int sqlType) {
		for (JDBCType type : JDBCType.values()) {
			if (type.getVendorTypeNumber().intValue() == sqlType) {
				return type;
			}
		}
		//下位互換のため、念のため生成して返す
		return new SQLType() {
			@Override
			public Integer getVendorTypeNumber() {
				return sqlType;
			}

			@Override
			public String getVendor() {
				return "unknown";
			}

			@Override
			public String getName() {
				return "unknown name:" + sqlType;
			}
		};
	}

	/**
	 * PreparedStatementへのパラメータセット処理
	 *
	 * @param preparedStatement PreparedStatement
	 * @param parameterIndex index
	 * @param param オリジナルパラメータ
	 * @param parameterMapperManager パラメータ変換管理クラス
	 * @throws SQLException SQL例外
	 */
	private void setParameterObject(final PreparedStatement preparedStatement, final int parameterIndex,
			final Object param, final BindParameterMapperManager parameterMapperManager) throws SQLException {
		//JDBCの受け付ける型に変換
		Object jdbcParam = parameterMapperManager.toJdbc(param, preparedStatement.getConnection());
		if (Objects.equals(sqlType, SQL_TYPE_NOT_SET)) {
			preparedStatement.setObject(parameterIndex, jdbcParam);
		} else {
			int targetSqlType = sqlType.getVendorTypeNumber();//各JDBCの対応状況が怪しいのでintで扱う
			if (jdbcParam != null) {
				preparedStatement.setObject(parameterIndex, jdbcParam, targetSqlType);
			} else {
				preparedStatement.setNull(parameterIndex, targetSqlType);
			}
		}
	}
}
