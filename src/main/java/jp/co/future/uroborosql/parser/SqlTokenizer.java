package jp.co.future.uroborosql.parser;

/**
 * SQL分割処理インターフェース
 *
 * @author H.Sugimoto
 */
public interface SqlTokenizer {
	/**
	 * 現在地取得
	 * @return
	 */
	int getPosition();

	/**
	 * トークン取得
	 *
	 * @return トークン
	 */
	String getToken();

	/**
	 * 以前のSQL文字列取得
	 * @return 以前のSQL文字列
	 */
	String getBefore();

	/**
	 * 以降のSQL文字列取得
	 * @return 以降のSQL文字列
	 */
	String getAfter();

	/**
	 * トークンの種別を取得
	 * @return トークンの種別
	 */
	TokenType getTokenType();

	/**
	 * 次のトークンの種別を取得
	 * @return 次のトークンの種別
	 */
	TokenType getNextTokenType();

	/**
	 * 次へ移動
	 * @return 次のトークンの種別
	 */
	TokenType next();

	/**
	 * トークンスキップ
	 * @return 次のトークン
	 */
	String skipToken();

	/**
	 * ホワイトスペーススキップ
	 * @return 次のトークン
	 */
	String skipWhitespace();

}