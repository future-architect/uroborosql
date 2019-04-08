/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import java.sql.JDBCType;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * Postgresql用のDialect
 *
 * @author H.Sugimoto
 */
public class PostgresqlDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public PostgresqlDialect() {
		super();
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
	 * @see jp.co.future.uroborosql.dialect.AbstractDialect#getJavaType(java.sql.JDBCType, java.lang.String)
	 */
	@Override
	public JavaType getJavaType(final JDBCType jdbcType, final String jdbcTypeName) {
		if (JDBCType.OTHER.equals(jdbcType)) {
			if ("json".equalsIgnoreCase(jdbcTypeName) || "jsonb".equalsIgnoreCase(jdbcTypeName)) {
				// JSON型の場合、文字列として扱う
				return super.getJavaType(JDBCType.NVARCHAR, jdbcTypeName);
			}
		}
		return super.getJavaType(jdbcType, jdbcTypeName);
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

}
