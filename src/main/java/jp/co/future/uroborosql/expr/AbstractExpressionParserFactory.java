/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.expr;

import java.lang.reflect.InvocationTargetException;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * 評価式パーサーのファクトリ抽象クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractExpressionParserFactory implements ExpressionParserFactory {

	/**
	 * 指定したクラスがClassLoader上に存在するかどうかを判定する.
	 *
	 * @param className 存在を確認するクラス名（FQDN）
	 * @return クラスが存在する場合<code>true</code>
	 */
	protected boolean existsTargetClass(final String className) {
		try {
			Class.forName(className);
			return true;
		} catch (ClassNotFoundException ex) {
			return false;
		}
	}

	/**
	 * ExpressionParserを生成.
	 *
	 * @param className 生成するExpressionParserクラス名(FQDN)
	 * @return 生成したExpressionParser
	 */
	protected ExpressionParser createExpressionParser(final String className) {
		try {
			return (ExpressionParser) Class.forName(className)
					.getConstructor().newInstance();
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException
				| NoSuchMethodException | SecurityException | ClassNotFoundException e) {
			throw new UroborosqlRuntimeException(className + " could not be created.", e);
		}
	}
}
