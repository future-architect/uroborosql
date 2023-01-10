package jp.co.future.uroborosql.event;

import java.sql.CallableStatement;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * Procedure実行後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class ProcedureEvent extends ExecutionEvent {
	/** 実行結果. */
	private boolean result;
	/** CallableStatement. */
	private final CallableStatement callableStatement;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param result 実行結果
	 * @param callableStatement CallableStatement
	 */
	public ProcedureEvent(final ExecutionContext executionContext,
			final boolean result,
			final CallableStatement callableStatement) {
		super(executionContext);
		this.result = result;
		this.callableStatement = callableStatement;
	}

	/**
	 * 実行結果の取得.
	 * @return 実行結果
	 */
	public boolean isResult() {
		return result;
	}

	/**
	 * 実行結果の設定.
	 * @param result 実行結果
	 */
	public void setResult(final boolean result) {
		this.result = result;
	}

	/**
	 * CallableStatementの取得.
	 * @return CallableStatement
	 */
	public CallableStatement getCallableStatement() {
		return callableStatement;
	}

}
