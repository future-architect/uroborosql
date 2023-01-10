package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * パラメータ設定前イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class BeforeSetParameterEvent extends ExecutionEvent {
	/** Parameter. */
	private Parameter parameter;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param parameter Parameter
	 */
	public BeforeSetParameterEvent(final ExecutionContext executionContext, final Parameter parameter) {
		super(executionContext);
		this.parameter = parameter;
	}

	/**
	 * Parameterの取得.
	 * @return Parameter
	 */
	public Parameter getParameter() {
		return parameter;
	}

	/**
	 * Parameterの設定.
	 * @param parameter Parameter
	 */
	public void setParameter(final Parameter parameter) {
		this.parameter = parameter;
	}

}
