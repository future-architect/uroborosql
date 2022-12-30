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

/**
 * H2用のDialect
 *
 * @author H.Sugimoto
 */
public class H2Dialect extends AbstractDialect {
	/**
	 * 悲観ロックのErrorCode もしくは SqlState. H2DBの場合はSqlStateで判定する.
	 * <pre>
	 * テーブル {0} のロック試行がタイムアウトしました
	 * Timeout trying to lock table {0}; SQL statement: [50200-199]
	 * </pre>
	 */
	private static final Set<String> pessimisticLockingErrorCodes = Collections.singleton("50200");

	/**
	 * コンストラクタ
	 */
	public H2Dialect() {
		super();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getDatabaseName()
	 */
	@Override
	public String getDatabaseName() {
		return "H2";
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
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsForUpdateNoWait()
	 */
	@Override
	public boolean supportsForUpdateNoWait() {
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
		return "nextval('" + sequenceName + "')";
	}

	@Override
	public StringBuilder addOptimizerHints(final StringBuilder sql, final List<String> hints) {
		var hintStr = "$1 USE INDEX " + hints.stream().collect(Collectors.joining(", ", "(", ")"))
				+ System.lineSeparator();
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
