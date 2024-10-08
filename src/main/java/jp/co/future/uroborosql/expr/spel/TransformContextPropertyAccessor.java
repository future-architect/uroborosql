/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.expr.spel;

import org.springframework.expression.AccessException;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.PropertyAccessor;
import org.springframework.expression.TypedValue;

import jp.co.future.uroborosql.parser.TransformContext;
import jp.co.future.uroborosql.utils.SqlFunction;

/**
 * TransformContextに対するプロパティアクセッサ
 *
 * @author H.Sugimoto
 */
public class TransformContextPropertyAccessor implements PropertyAccessor {
	/** SqlFunction */
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
	 * @see org.springframework.expression.PropertyAccessor#getSpecificTargetClasses()
	 */
	@Override
	public Class<?>[] getSpecificTargetClasses() {
		return new Class<?>[] { TransformContext.class };
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.springframework.expression.PropertyAccessor#canRead(org.springframework.expression.EvaluationContext, java.lang.Object, java.lang.String)
	 */
	@Override
	public boolean canRead(final EvaluationContext context, final Object target, final String name)
			throws AccessException {
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.springframework.expression.PropertyAccessor#read(org.springframework.expression.EvaluationContext, java.lang.Object, java.lang.String)
	 */
	@Override
	public TypedValue read(final EvaluationContext context, final Object target, final String name)
			throws AccessException {
		if (SqlFunction.SHORT_NAME.equals(name)) {
			return new TypedValue(sqlFunction);
		} else {
			var ctx = (TransformContext) target;
			var param = ctx.getParam(name);
			if (param == null) {
				return new TypedValue(null);
			}
			return new TypedValue(param.getValue());
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.springframework.expression.PropertyAccessor#canWrite(org.springframework.expression.EvaluationContext, java.lang.Object, java.lang.String)
	 */
	@Override
	public boolean canWrite(final EvaluationContext context, final Object target, final String name)
			throws AccessException {
		return false;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.springframework.expression.PropertyAccessor#write(org.springframework.expression.EvaluationContext, java.lang.Object, java.lang.String, java.lang.Object)
	 */
	@Override
	public void write(final EvaluationContext context, final Object target, final String name, final Object newValue)
			throws AccessException {
		throw new UnsupportedOperationException();
	}

}
