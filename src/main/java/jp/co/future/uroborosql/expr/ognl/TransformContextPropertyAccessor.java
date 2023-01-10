/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.expr.ognl;

import java.util.Map;

import jp.co.future.uroborosql.parser.TransformContext;
import jp.co.future.uroborosql.utils.SqlFunction;
import ognl.ObjectPropertyAccessor;
import ognl.OgnlException;

/**
 * {@link TransformContext}クラスのOGNLコンテキスト上でのプロパティアクセサ
 *
 * @author H.Sugimoto
 */
public class TransformContextPropertyAccessor extends ObjectPropertyAccessor {

	private final SqlFunction sqlFunction;

	/**
	 * コンストラクタ
	 *
	 * @param sqlFunction sqlFunction
	 */
	public TransformContextPropertyAccessor(final SqlFunction sqlFunction) {
		this.sqlFunction = sqlFunction;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see ognl.ObjectPropertyAccessor#getProperty(java.util.Map, java.lang.Object, java.lang.Object)
	 */
	@Override
	@SuppressWarnings("rawtypes")
	public Object getProperty(final Map cx, final Object target, final Object name) throws OgnlException {
		if (SqlFunction.SHORT_NAME.equals(name)) {
			return sqlFunction;
		} else {
			var context = (TransformContext) target;
			var param = context.getParam((String) name);
			if (param == null) {
				return null;
			}
			return param.getValue();
		}
	}
}
