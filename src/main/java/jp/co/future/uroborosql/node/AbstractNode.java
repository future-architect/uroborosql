package jp.co.future.uroborosql.node;

import java.util.ArrayList;
import java.util.List;

/**
 * SQLノードの抽象親クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractNode implements Node {

	/** 子ノード */
	private final List<Node> children = new ArrayList<>();

	/**
	 * 保持しているSQLが適用対象となった場合に<code>PASSED</code>となる.
	 * SQL文のカバレッジ取得に利用
	 */
	protected CoverageState state = CoverageState.FAILED;

	/**
	 * コンストラクタ
	 */
	public AbstractNode() {
		// do nothing
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
	 * @see jp.co.future.uroborosql.node.Node#passed(java.lang.StringBuilder)
	 */
	@Override
	public void passed(final StringBuilder builder) {
		// do nothing
	}
}
