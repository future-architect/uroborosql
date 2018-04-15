/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

/**
 * Dialectの抽象親クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractDialect implements Dialect {
	private final String dialectName = this.getClass().getSimpleName().replace("Dialect", "").toLowerCase();

	/**
	 * Dialect識別用の文字列を取得する
	 *
	 * @return Dialect識別用文字列
	 */
	@Override
	public String getDialectName() {
		return dialectName;
	}

}
