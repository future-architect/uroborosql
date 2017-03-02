package jp.co.future.uroborosql.context;

import java.util.Map;

import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parser.TransformContext;
import ognl.ObjectPropertyAccessor;
import ognl.OgnlException;

/**
 * {@link TransformContext}クラスのOGNLコンテキスト上でのプロパティアクセサ
 *
 * @author H.Sugimoto
 */
public class TransformContextPropertyAccessor extends ObjectPropertyAccessor {

	/**
	 * {@inheritDoc}
	 *
	 * @see ognl.ObjectPropertyAccessor#getProperty(java.util.Map, java.lang.Object, java.lang.Object)
	 */
	@Override
	@SuppressWarnings("rawtypes")
	public Object getProperty(final Map cx, final Object target, final Object name) throws OgnlException {
		TransformContext context = (TransformContext) target;
		Parameter param = context.getParam((String) name);
		if (param == null) {
			return null;
		}
		return param.getValue();
	}
}
