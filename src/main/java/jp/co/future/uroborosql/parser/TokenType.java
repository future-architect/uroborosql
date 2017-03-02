package jp.co.future.uroborosql.parser;

/**
 * トークン種別
 *
 * @author H.Sugimoto
 */
enum TokenType {
	/** トークン種別：SQL */
	SQL,

	/** トークン種別：コメント */
	COMMENT,

	/** トークン種別：ELSE */
	ELSE,

	/** トークン種別：バインド変数 */
	BIND_VARIABLE,

	/** トークン種別：EOF */
	EOF,
}