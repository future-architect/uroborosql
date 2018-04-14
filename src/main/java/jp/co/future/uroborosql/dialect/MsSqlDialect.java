/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

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

	@Override
	public String getName() {
		return "Microsoft SQL Server";
	}

	/**
	 * MSSQLではMerge文で;を使用するため終端文字の削除を行わない
	 * @return false
	 */
	@Override
	public boolean isRemoveTerminator() {
		return false;
	}
}
