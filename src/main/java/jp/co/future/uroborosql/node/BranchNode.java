package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.coverage.BranchCoverageState;
import jp.co.future.uroborosql.parser.TransformContext;

/**
 * 分岐Node
 *
 * @author ota
 */
public class BranchNode extends ContainerNode {

	/**
	 * ブランチ分岐ステータス. SQL文のカバレッジ取得に利用
	 */
	private BranchCoverageState state = BranchCoverageState.NOT_PASSED;

	/**
	 * コンストラクタ
	 *
	 * @param position 開始位置
	 */
	public BranchNode(int position) {
		super(position);
	}

	@Override
	public void accept(final TransformContext transformContext) {
		super.accept(transformContext);
	}

	/**
	 * 本クラスを継承したNodeは<br>
	 * accept()時に必ず更新すること
	 *
	 * @param bool 分岐結果
	 */
	protected void passState(boolean bool) {
		pass();
		this.state = bool ? BranchCoverageState.TRUE : BranchCoverageState.FALSE;
	}

	protected BranchCoverageState getState() {
		return state;
	}
}
