package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parser.TransformContext;

/**
 * SQLノードインタフェース
 *
 * @author H.Sugimoto
 */
public interface Node {
	/**
	 * カバレッジの状態を表現する列挙体
	 */
	enum CoverageState {
		/** 通過 */
		PASSED(1),
		/** 未通過 */
		FAILED(0);

		private final int val;

		private CoverageState(final int val) {
			this.val = val;
		}

		@Override
		public String toString() {
			return String.valueOf(val);
		}
	}

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

	/**
	 * このノードが有効になっているかどうか。結果は引数のbuilderにCoverageState(PASSED/FAILED)をappendする。
	 *
	 * @param builder 結果を追記するbuilder
	 */
	void passed(StringBuilder builder);

}
