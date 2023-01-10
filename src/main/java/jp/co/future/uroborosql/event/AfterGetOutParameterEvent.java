package jp.co.future.uroborosql.event;

import java.sql.CallableStatement;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * 出力パラメータ取得後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class AfterGetOutParameterEvent extends ExecutionEvent {
	/** 出力パラメータ名. */
	private final String key;
	/** 出力パラメータの値. */
	private Object value;
	/** CallableStatement. */
	private final CallableStatement callableStatement;
	/** パラメータインデックス. */
	private final int parameterIndex;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param key 出力パラメータ名
	 * @param value 出力パラメータの値
	 * @param callableStatement CallableStatement
	 * @param parameterIndex パラメータインデックス
	 */
	public AfterGetOutParameterEvent(final ExecutionContext executionContext,
			final String key,
			final Object value,
			final CallableStatement callableStatement,
			final int parameterIndex) {
		super(executionContext);
		this.key = key;
		this.value = value;
		this.callableStatement = callableStatement;
		this.parameterIndex = parameterIndex;
	}

	/**
	 * 出力パラメータ名の取得.
	 * @return 出力パラメータ名
	 */
	public String getKey() {
		return key;
	}

	/**
	 * 出力パラメータの値の取得.
	 * @return 出力パラメータの値
	 */
	public Object getValue() {
		return value;
	}

	/**
	 * 出力パラメータの値の設定.
	 * @param value 出力パラメータの値
	 */
	public void setValue(final Object value) {
		this.value = value;
	}

	/**
	 * CallableStatementの取得.
	 * @return CallableStatement
	 */
	public CallableStatement getCallableStatement() {
		return callableStatement;
	}

	/**
	 * パラメータインデックスの取得.
	 * @return パラメータインデックス
	 */
	public int getParameterIndex() {
		return parameterIndex;
	}

}
