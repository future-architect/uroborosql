package jp.co.future.uroborosql.parser;

/**
 * SQL解析インターフェース
 *
 * @author H.Sugimoto
 */
public interface SqlParser {
	/**
	 * SQL解析<br>
	 * SQL文の内容を解析し、変換クラスを生成する
	 *
	 * @return コンテキスト変換器
	 */
	ContextTransformer parse();
}