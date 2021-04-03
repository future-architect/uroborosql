/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import java.sql.JDBCType;
import java.sql.SQLType;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * Postgresql用のDialect
 *
 * @author H.Sugimoto
 */
public class PostgresqlDialect extends AbstractDialect {
	/**
	 * 悲観ロックのErrorCode もしくは SqlState. Postgresqlの場合はSqlStateで判定する.
	 * <pre>Error [55P03]: ERROR: リレーション"XXX"の行ロックを取得できませんでした</pre>
	 */
	private static final Set<String> pessimisticLockingErrorCodes = Collections.singleton("55P03");

	/**
	 * コンストラクタ
	 */
	public PostgresqlDialect() {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#isRollbackToSavepointBeforeRetry()
	 */
	@Override
	public boolean isRollbackToSavepointBeforeRetry() {
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getDatabaseName()
	 */
	@Override
	public String getDatabaseName() {
		return "PostgreSQL";
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsBulkInsert()
	 */
	@Override
	public boolean supportsBulkInsert() {
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsLimitClause()
	 */
	@Override
	public boolean supportsLimitClause() {
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsNullValuesOrdering()
	 */
	@Override
	public boolean supportsNullValuesOrdering() {
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsForUpdateWait()
	 */
	@Override
	public boolean supportsForUpdateWait() {
		return false;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsOptimizerHints()
	 */
	@Override
	public boolean supportsOptimizerHints() {
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.AbstractDialect#getJavaType(java.sql.SQLType, java.lang.String)
	 */
	@Override
	public JavaType getJavaType(final SQLType sqlType, final String sqlTypeName) {
		return this.getJavaType(sqlType.getVendorTypeNumber(), sqlTypeName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.AbstractDialect#getJavaType(int, java.lang.String)
	 */
	@Override
	public JavaType getJavaType(final int sqlType, final String sqlTypeName) {
		if (JDBCType.OTHER.getVendorTypeNumber().equals(sqlType)) {
			if ("json".equalsIgnoreCase(sqlTypeName) || "jsonb".equalsIgnoreCase(sqlTypeName)) {
				// JSON型の場合、文字列として扱う
				return super.getJavaType(JDBCType.NVARCHAR, sqlTypeName);
			}
		} else if ("timestamptz".equalsIgnoreCase(sqlTypeName)
				&& JDBCType.TIMESTAMP.getVendorTypeNumber().equals(sqlType)) {
			return super.getJavaType(JDBCType.TIMESTAMP_WITH_TIMEZONE, sqlTypeName);
		} else if ("timetz".equalsIgnoreCase(sqlTypeName)
				&& JDBCType.TIME.getVendorTypeNumber().equals(sqlType)) {
			return super.getJavaType(JDBCType.TIME_WITH_TIMEZONE, sqlTypeName);
		}
		return super.getJavaType(sqlType, sqlTypeName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getSequenceNextValSql(java.lang.String)
	 */
	@Override
	public String getSequenceNextValSql(final String sequenceName) {
		return "nextval('" + sequenceName + "')";
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#addOptimizerHints(java.lang.StringBuilder, java.util.List)
	 */
	@Override
	public StringBuilder addOptimizerHints(final StringBuilder sql, final List<String> hints) {
		var hintStr = "/*+" + hints.stream().collect(Collectors.joining(System.lineSeparator() + "\t",
				System.lineSeparator() + "\t", System.lineSeparator())) + " */" + System.lineSeparator();
		return sql.insert(0, hintStr);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getPessimisticLockingErrorCodes()
	 */
	@Override
	public Set<String> getPessimisticLockingErrorCodes() {
		return pessimisticLockingErrorCodes;
	}
}
