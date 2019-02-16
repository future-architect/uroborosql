/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import jp.co.future.uroborosql.connection.ConnectionSupplier;

/**
 * Oracle12（以降のバージョンも含む）用のDialect
 *
 * @author H.Sugimoto
 */
public class Oracle12Dialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public Oracle12Dialect() {
		super('\\', new char[] { '%', '_', '％', '＿' });
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
	 * @see jp.co.future.uroborosql.dialect.Dialect#accept(jp.co.future.uroborosql.connection.ConnectionSupplier)
	 */
	@Override
	public boolean accept(final ConnectionSupplier supplier) {
		if (supplier == null) {
			return false;
		}

		String[] parts = supplier.getDatabaseName().split("-", 2);
		String databaseName = parts[0];

		if (!databaseName.startsWith(getDatabaseName())) {
			return false;
		}

		String databaseVersion = parts[1];

		try {
			int majorVersion = Integer.parseInt(databaseVersion.substring(0, databaseVersion.indexOf(".")));
			return majorVersion >= 12;
		} catch (NumberFormatException e) {
			return false;
		}
	}
}
