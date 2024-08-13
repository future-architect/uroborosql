/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * MariaDB用のデフォルト設定用Dialect
 *
 * @author H.Sugimoto
 */
public abstract class MariaDbDialect extends AbstractDialect {
	/**
	 * 悲観ロックのErrorCode もしくは SqlState. MySQLの場合はErrorCodeで判定する.
	 * <pre>ERROR 3572 (HY000): Statement aborted because lock(s) could not be acquired immediately and NOWAIT is set.</pre>
	 */
	private static final Set<String> pessimisticLockingErrorCodes = Set.of("3572");

	/**
	 * コンストラクタ
	 */
	protected MariaDbDialect() {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#accept(jp.co.future.uroborosql.connection.ConnectionSupplier)
	 */
	@Override
	public boolean accept(final ConnectionSupplier supplier) {
		if (supplier == null) {
			return false;
		}

		var parts = supplier.getDatabaseName().split("-", 2);
		var databaseName = parts[0];

		if (!databaseName.startsWith(getDatabaseName())) {
			return false;
		}

		var databaseVersion = parts[1];

		try {
			var majorVersion = Integer.parseInt(databaseVersion.substring(0, databaseVersion.indexOf(".")));
			return isTargetVersion(majorVersion);
		} catch (NumberFormatException ex) {
			return false;
		}
	}

	/**
	 * 対象のMariaDBバージョンかどうかを判定する
	 *
	 * @param majorVersion コネクションから取得したメジャーバージョン
	 * @return 対象のバージョンの場合<code>true</code>
	 */
	protected abstract boolean isTargetVersion(int majorVersion);

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getDatabaseName()
	 */
	@Override
	public String getDatabaseName() {
		return "MariaDB";
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
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsSequence()
	 */
	@Override
	public boolean supportsSequence() {
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
		throw new UroborosqlRuntimeException("MariaDB does not support Sequence.");
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#addOptimizerHints(java.lang.StringBuilder, java.util.List)
	 */
	@Override
	public StringBuilder addOptimizerHints(final StringBuilder sql, final List<String> hints) {
		var hintStr = "$1 " + hints.stream().collect(Collectors.joining(" ")) + System.lineSeparator();
		return new StringBuilder(sql.toString().replaceFirst("((FROM|from)\\s+[^\\s]+)\\s*", hintStr));
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
