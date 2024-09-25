/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter;

import java.sql.JDBCType;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLType;
import java.util.Map;
import java.util.Objects;

import jp.co.future.uroborosql.log.support.ServiceLoggingSupport;
import jp.co.future.uroborosql.log.support.SqlLoggingSupport;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;
import jp.co.future.uroborosql.utils.ObjectUtils;

/**
 * パラメータオブジェクト。<br>
 * SQLへバインドするパラメータを保持する。<br>
 *
 * @author H.Sugimoto
 */
public class Parameter implements ServiceLoggingSupport, SqlLoggingSupport {
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
		var subParameterName = parameterName + "." + propertyName;
		Object subValue = null;
		if (value != null) {
			if (value instanceof Map) {
				subValue = ((Map) value).get(propertyName);
				if (subValue == null) {
					warnWith(LOG)
							.setMessage("Set subparameter value to NULL because property can not be accessed.[{}]")
							.addArgument(subParameterName)
							.log();
				}
			} else {
				try {
					// フィールドアクセスで値の取得を実施
					var field = value.getClass().getDeclaredField(propertyName);
					field.setAccessible(true);
					subValue = field.get(value);
				} catch (NoSuchFieldException ex) {
					// メソッドアクセスで値の取得を実施
					try {
						var prefix = boolean.class.equals(value.getClass()) ? "is" : "get";
						var method = value.getClass()
								.getMethod(prefix + ObjectUtils.capitalize(propertyName));
						subValue = method.invoke(value);
					} catch (Exception ex2) {
						warnWith(LOG)
								.setMessage("Set subparameter value to NULL because property can not be accessed.[{}]")
								.addArgument(subParameterName)
								.setCause(ex2)
								.log();
					}
				} catch (Exception ex) {
					warnWith(LOG)
							.setMessage("Set subparameter value to NULL because property can not be accessed.[{}]")
							.addArgument(subParameterName)
							.setCause(ex)
							.log();
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
	protected int setInParameter(final PreparedStatement preparedStatement, final int index,
			final BindParameterMapperManager parameterMapperManager) throws SQLException {
		var parameterIndex = index;
		if (value instanceof Iterable) {
			for (var e : (Iterable<?>) value) {
				setParameterObject(preparedStatement, parameterIndex, e, parameterMapperManager);

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
		if (SQL_LOG.isDebugEnabled() && !isSuppressParameterLogging()) {
			debugWith(SQL_LOG)
					.setMessage("Set the parameter.[INDEX[{}], {}]")
					.addArgument(index)
					.addArgument(this)
					.log();
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
		return "Parameter name[" + parameterName + "], Value[" + value + "], Class["
				+ (value == null ? "NULL" : value.getClass().getSimpleName())
				+ (Objects.equals(sqlType, SQL_TYPE_NOT_SET) ? "]" : "], SQL type[" + sqlType.getName() + "]");
	}

	/**
	 * {@link java.sql.Types} の値を {@link java.sql.SQLType} に変換する
	 * @param sqlType {@link java.sql.Types} の値
	 * @return {@link java.sql.SQLType} の値
	 */
	private SQLType toSqlType(final int sqlType) {
		for (var type : JDBCType.values()) {
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
		var jdbcParam = parameterMapperManager.toJdbc(param, preparedStatement.getConnection());
		if (Objects.equals(sqlType, SQL_TYPE_NOT_SET)) {
			if (jdbcParam instanceof java.sql.Array) {
				preparedStatement.setArray(parameterIndex, (java.sql.Array) jdbcParam);
			} else {
				if (jdbcParam != null) {
					preparedStatement.setObject(parameterIndex, jdbcParam);
				} else {
					preparedStatement.setNull(parameterIndex, JDBCType.NULL.getVendorTypeNumber());
				}
			}
		} else {
			int targetSqlType = sqlType.getVendorTypeNumber();//各JDBCの対応状況が怪しいのでintで扱う
			if (jdbcParam != null) {
				if (jdbcParam instanceof java.sql.Array) {
					preparedStatement.setArray(parameterIndex, (java.sql.Array) jdbcParam);
				} else {
					preparedStatement.setObject(parameterIndex, jdbcParam, targetSqlType);
				}
			} else {
				preparedStatement.setNull(parameterIndex, targetSqlType);
			}
		}
	}
}
