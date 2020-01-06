/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.expr.ognl;

import java.util.Map;

import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parser.TransformContext;
import jp.co.future.uroborosql.utils.StringFunction;
import ognl.ObjectPropertyAccessor;
import ognl.OgnlException;

/**
 * {@link TransformContext}クラスのOGNLコンテキスト上でのプロパティアクセサ
 *
 * @author H.Sugimoto
 */
public class TransformContextPropertyAccessor extends ObjectPropertyAccessor {

	private final StringFunction expressionFunction;

	/**
	 * コンストラクタ
	 *
	 * @param expressionFunction expressionFunction
	 */
	public TransformContextPropertyAccessor(final StringFunction expressionFunction) {
		this.expressionFunction = expressionFunction;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see ognl.ObjectPropertyAccessor#getProperty(java.util.Map, java.lang.Object, java.lang.Object)
	 */
	@Override
	@SuppressWarnings("rawtypes")
	public Object getProperty(final Map cx, final Object target, final Object name) throws OgnlException {
		if (StringFunction.SHORT_NAME.equals(name)) {
			return expressionFunction;
		} else {
			TransformContext context = (TransformContext) target;
			Parameter param = context.getParam((String) name);
			if (param == null) {
				return null;
			}
			return param.getValue();
		}
	}
}
