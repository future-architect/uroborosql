package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parser.TransformContext;

/**
 * SQL文を表すノード.
 * 前にAND または ORがある場合に利用される
 *
 * @author H.Sugimoto
 */
public class PrefixSqlNode extends AbstractNode {

	/** プレフィックス "AND" または "OR"のいづれかが設定される */
	private final String prefix;

	/** SQL文 */
	private final String sqlPart;

	/**
	 * 保持しているSQLが適用対象となった場合に<code>true</code>となる.
	 * SQL文のカバレッジ取得に利用
	 */
	private boolean passed = false;

	/**
	 * コンストラクタ
	 *
	 * @param prefix プレフィックス
	 * @param sqlPart SQL文
	 */
	public PrefixSqlNode(final String prefix, final String sqlPart) {
		this.prefix = prefix;
		this.sqlPart = sqlPart;
	}

	/**
	 * このノードが有効になっているかどうか
	 * @return 有効になっている場合<code>true</code>
	 */
	public boolean isPassed() {
		return passed;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		if (transformContext.isEnabled()) {
			transformContext.addSqlPart(prefix);
		}
		transformContext.addSqlPart(sqlPart);
		passed = true;
	}

}
