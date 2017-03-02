package jp.co.future.uroborosql.context;

import java.util.Map;

import jp.co.future.uroborosql.parameter.Parameter;
import ognl.ObjectPropertyAccessor;
import ognl.OgnlException;

/**
 * {@link Parameter}クラスのOGNLコンテキスト上でのプロパティアクセサ
 *
 * @author H.Sugimoto
 */
public class ParameterPropertyAccessor extends ObjectPropertyAccessor {

	/**
	 * {@inheritDoc}
	 *
	 * @see ognl.ObjectPropertyAccessor#getProperty(java.util.Map, java.lang.Object, java.lang.Object)
	 */
	@SuppressWarnings("rawtypes")
	@Override
	public Object getProperty(final Map cx, final Object target, final Object name) throws OgnlException {
		return target == null ? null : ((Parameter) target).getValue();
	}
}
