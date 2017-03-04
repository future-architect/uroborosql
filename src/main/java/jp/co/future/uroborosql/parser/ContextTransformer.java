package jp.co.future.uroborosql.parser;

import jp.co.future.uroborosql.node.Node;

/**
 * Transformコンテキストの変換器
 *
 * @author H.Sugimoto
 *
 */
public class ContextTransformer {
	/** ルートノード */
	private final Node root;

	/**
	 * コンストラクタ
	 *
	 * @param root ルートノード
	 */
	public ContextTransformer(final Node root) {
		this.root = root;
	}

	/**
	 * Transformコンテキストの変換を実施する
	 *
	 * @param transformContext Transformコンテキスト
	 */
	public void transform(final TransformContext transformContext) {
		root.accept(transformContext);
	}

	/**
	 * ルートノードの取得
	 *
	 * @return ルートノード
	 */
	public Node getRoot() {
		return root;
	}

	/**
	 * 変換したSQLで評価された分岐を表す文字列を取得する
	 *
	 * @return 評価結果を表す文字列
	 */
	public String getPassedRoute() {
		StringBuilder builder = new StringBuilder();
		root.passed(builder);
		// 分岐がない場合、全ルートをパスした意味で1を返す
		return builder.length() > 0 ? builder.toString() : "1";
	}

}
