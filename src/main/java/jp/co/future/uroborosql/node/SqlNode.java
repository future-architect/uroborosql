package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parser.TransformContext;

/**
 * SQL文を表すノード
 *
 * @author H.Sugimoto
 */
public class SqlNode extends AbstractNode {

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
	 * @param sqlPart SQL文
	 */
	public SqlNode(final String sqlPart) {
		this.sqlPart = sqlPart;
	}

	/**
	 * SQL文の取得
	 *
	 * @return SQL文
	 */
	public String getSql() {
		return sqlPart;
	}

	/**
	 * このノードが有効になっているかどうか
	 *
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
		transformContext.addSqlPart(sqlPart);
		passed = true;
	}

}
