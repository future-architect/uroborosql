package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parser.TransformContext;

/**
 * SQLノードインタフェース
 *
 * @author H.Sugimoto
 */
public interface Node {

	/**
	 * 子ノード数取得
	 *
	 * @return 子ノード数
	 */
	int getChildSize();

	/**
	 * 子ノード取得
	 *
	 * @param index インデックス
	 * @return 子ノード
	 */
	Node getChild(int index);

	/**
	 * 子ノード追加
	 *
	 * @param node 子ノード
	 */
	void addChild(Node node);

	/**
	 * TransformContextの編集処理
	 *
	 * @param transformContext Transformコンテキスト
	 */
	void accept(TransformContext transformContext);

}
