/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * MySQL用のDialect
 *
 * @author H.Sugimoto
 */
public class MySqlDialect extends AbstractDialect {
	/**
	 * 悲観ロックのErrorCode もしくは SqlState. MySQLの場合はErrorCodeで判定する.
	 * <pre>ERROR 3572 (HY000): Statement aborted because lock(s) could not be acquired immediately and NOWAIT is set.</pre>
	 */
	private static final Set<String> pessimisticLockingErrorCodes = Collections.singleton("3572");

	/**
	 * コンストラクタ
	 */
	public MySqlDialect() {
		super();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getDatabaseName()
	 */
	@Override
	public String getDatabaseName() {
		return "MySQL";
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
		throw new UroborosqlRuntimeException("MySql does not support Sequence.");
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#addOptimizerHints(java.lang.StringBuilder, java.util.List)
	 */
	@Override
	public StringBuilder addOptimizerHints(final StringBuilder sql, final List<String> hints) {
		String hintStr = "$1 " + hints.stream().collect(Collectors.joining(" ")) + System.lineSeparator();
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
