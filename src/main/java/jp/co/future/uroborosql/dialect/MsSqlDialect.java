/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.enums.ForUpdateType;

/**
 * Microsoft SQLServer用のDialect
 *
 * @author H.Sugimoto
 */
public class MsSqlDialect extends AbstractDialect {
	/**
	 * 悲観ロックのErrorCode もしくは SqlState. MSSQLの場合はErrorCodeで判定する.
	 * <pre>SQL Error [1222] [S00045]: ロック要求がタイムアウトしました。 </pre>
	 */
	private static final Set<String> pessimisticLockingErrorCodes = Collections.singleton("1222");

	/**
	 * コンストラクタ
	 */
	public MsSqlDialect() {
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
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsOptimizerHints()
	 */
	@Override
	public boolean supportsOptimizerHints() {
		return true;
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
		var hints = new ArrayList<String>();
		hints.add("UPDLOCK");
		hints.add("ROWLOCK");
		if (forUpdateType == ForUpdateType.NOWAIT) {
			hints.add("NOWAIT");
		}
		var forUpdate = "$1 WITH " + hints.stream()
				.collect(Collectors.joining(", ", "(", ")")) + System.lineSeparator();
		return new StringBuilder(sql.toString().replaceFirst("((FROM|from)\\s+[^\\s]+)\\s*", forUpdate));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#addOptimizerHints(java.lang.StringBuilder, java.util.List)
	 */
	@Override
	public StringBuilder addOptimizerHints(final StringBuilder sql, final List<String> hints) {
		if (sql.indexOf("WITH (") > 0) {
			// forUpdateのヒントが追加されている場合
			var hintStr = "WITH ($1, " + hints.stream()
					.collect(Collectors.joining(", ")) + ")";
			return new StringBuilder(sql.toString().replaceFirst("WITH \\((.+)\\)", hintStr));
		} else {
			var hintStr = "$1 WITH " + hints.stream()
					.collect(Collectors.joining(", ", "(", ")")) + System.lineSeparator();
			return new StringBuilder(sql.toString().replaceFirst("((FROM|from)\\s+[^\\s]+)\\s*", hintStr));
		}
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
