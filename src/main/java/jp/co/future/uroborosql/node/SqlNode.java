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
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#accept(jp.co.future.uroborosql.parser.TransformContext)
	 */
	@Override
	public void accept(final TransformContext transformContext) {
		transformContext.addSqlPart(sqlPart);
		state = CoverageState.PASSED;
	}

}
