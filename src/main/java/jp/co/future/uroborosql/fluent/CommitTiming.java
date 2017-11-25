package jp.co.future.uroborosql.fluent;

/**
 * コミットタイミング
 */
public enum CommitTiming {
	/** 何もしない */
	DO_NOTHING,
	/** バッチ実行毎にコミット */
	COMMIT_EACH_TIME;
}