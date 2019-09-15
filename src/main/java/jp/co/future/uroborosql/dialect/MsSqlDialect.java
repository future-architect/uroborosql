/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import jp.co.future.uroborosql.enums.ForUpdateType;

/**
 * Microsoft SQLServer用のDialect
 *
 * @author H.Sugimoto
 */
public class MsSqlDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public MsSqlDialect() {
		super();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getDatabaseName()
	 */
	@Override
	public String getDatabaseName() {
		return "Microsoft SQL Server";
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.AbstractDialect#getDatabaseType()
	 */
	@Override
	public String getDatabaseType() {
		return "mssql";
	}

	/**
	 * {@inheritDoc}
	 *
	 * <br>MSSQLではMerge文で;を使用するため終端文字の削除を行わない
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#isRemoveTerminator()
	 *
	 * @return false
	 */
	@Override
	public boolean isRemoveTerminator() {
		return false;
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
	 * @see jp.co.future.uroborosql.dialect.Dialect#getSequenceNextValSql(java.lang.String)
	 */
	@Override
	public String getSequenceNextValSql(final String sequenceName) {
		return "next value for " + sequenceName;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.AbstractDialect#addForUpdateClause(java.lang.StringBuilder, jp.co.future.uroborosql.enums.ForUpdateType, int)
	 */
	@Override
	public StringBuilder addForUpdateClause(final StringBuilder sql, final ForUpdateType forUpdateType,
			final int waitSeconds) {
		StringBuilder forUpdate = new StringBuilder("$1 WITH (UPDLOCK, ROWLOCK");
		if (forUpdateType == ForUpdateType.NORMAL) {
			forUpdate.append(") ");
		} else if (forUpdateType == ForUpdateType.NOWAIT) {
			forUpdate.append(", NOWAIT) ");
		}

		String origSql = sql.toString();
		return new StringBuilder(origSql.replaceFirst("((FROM|from)\\s+\\w+)\\s+", forUpdate.toString()));

	}
}
