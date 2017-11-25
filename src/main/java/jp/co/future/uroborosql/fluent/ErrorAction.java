package jp.co.future.uroborosql.fluent;

/**
 * エラー発生時動作
 */
public enum ErrorAction {
	/** 何もしない */
	DO_NOTHING,
	/** 例外をスローする */
	THROW_EXCEPTION;
}