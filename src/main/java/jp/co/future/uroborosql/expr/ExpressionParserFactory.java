package jp.co.future.uroborosql.expr;

/**
 * 評価式パーサーファクトリクラスインタフェース
 *
 * @author H.Sugimoto
 */
public interface ExpressionParserFactory {
	/**
	 * ExpressionParserが利用可能かどうかを取得する.
	 *
	 * @return 利用可能な場合<code>true</code>
	 */
	boolean accept();

	/**
	 * ExpressionParserの生成.
	 *
	 * @return ExpressionParser
	 */
	ExpressionParser create();
}
