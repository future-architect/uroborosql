package jp.co.future.uroborosql.node;

import java.util.ArrayList;
import java.util.List;

import jp.co.future.uroborosql.coverage.PassedRoute;

/**
 * SQLノードの抽象親クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractNode implements Node {

	/** 子ノード */
	private final List<Node> children = new ArrayList<>();

	/** ポジション */
	private final int position;

	/**
	 * 保持しているSQLが適用対象となった場合に<code>true</code>となる. SQL文のカバレッジ取得に利用
	 */
	private boolean passed = false;

	/**
	 * コンストラクタ
	 *
	 * @param position 開始位置
	 */
	public AbstractNode(int position) {
		this.position = position;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#getChildSize()
	 */
	@Override
	public int getChildSize() {
		return children.size();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#getChild(int)
	 */
	@Override
	public Node getChild(final int index) {
		return children.get(index);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#addChild(jp.co.future.uroborosql.node.Node)
	 */
	@Override
	public void addChild(final Node node) {
		children.add(node);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#passed(PassedRoute)
	 */
	@Override
	public void passed(final PassedRoute passed) {
		// do nothing
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#getPosition()
	 */
	@Override
	public int getPosition() {
		return this.position;
	}

	protected void pass() {
		passed = true;
	}

	protected boolean isPassed() {
		return passed;
	}

}
