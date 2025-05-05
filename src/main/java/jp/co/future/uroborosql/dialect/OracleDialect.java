/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.connection.ConnectionSupplier;

/**
 * Oracle共通の抽象Dialect
 *
 * @author H.Sugimoto
 */
public abstract class OracleDialect extends AbstractDialect {
	/**
	 * 悲観ロックのErrorCode もしくは SqlState. Oracleの場合errorCodeで判定する.
	 * <pre>ORA-00054: リソース・ビジー。NOWAITが指定されているか、タイムアウトしました</pre>
	 * <pre>ORA-30006: リソース・ビジー; WAITタイムアウトの期限に達しました。</pre>
	 */
	private static final Set<String> pessimisticLockingErrorCodes = Collections
			.unmodifiableSet(new HashSet<>(List.of("54", "30006")));

	/**
	 * コンストラクタ
	 * @param escapeChar like検索時のエスケープ文字
	 * @param wildcards like検索時のワイルドカード文字配列
	 */
	protected OracleDialect(final char escapeChar, final char[] wildcards) {
		super(escapeChar, wildcards);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getDatabaseName()
	 */
	@Override
	public String getDatabaseName() {
		return "Oracle";
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
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsIdentity()
	 */
	@Override
	public boolean supportsIdentity() {
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
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsUpdateChained()
	 */
	@Override
	public boolean supportsUpdateChained() {
		return false;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getSequenceNextValSql(java.lang.String)
	 */
	@Override
	public String getSequenceNextValSql(final String sequenceName) {
		return sequenceName + ".nextval";
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
	 * 対象のOracleバージョンかどうかを判定する
	 *
	 * @param majorVersion コネクションから取得したメジャーバージョン
	 * @return 対象のバージョンの場合<code>true</code>
	 */
	protected abstract boolean isTargetVersion(int majorVersion);

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getModLiteral(java.lang.String, java.lang.String)
	 */
	@Override
	public String getModLiteral(final String dividend, final String divisor) {
		return "MOD(" + dividend + ", " + divisor + ")";
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#addOptimizerHints(java.lang.StringBuilder, java.util.List)
	 */
	@Override
	public StringBuilder addOptimizerHints(final StringBuilder sql, final List<String> hints) {
		var hintStr = "$1 /*+ " + hints.stream().collect(Collectors.joining(" ")) + " */";
		return new StringBuilder(sql.toString().replaceFirst("(SELECT( /\\*.+\\*/)*)", hintStr));
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
