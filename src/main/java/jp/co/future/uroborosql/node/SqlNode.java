package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.coverage.CoverageState;
import jp.co.future.uroborosql.coverage.PassedRoute;
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
	 * @param startPosition 開始位置
	 * @param sqlPart SQL文
	 */
	public SqlNode(final int startPosition, final String sqlPart) {
		super(startPosition);
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
		pass();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.node.Node#passed(PassedRoute)
	 */
	@Override
	public void passed(final PassedRoute passed) {
		if (getState() == CoverageState.PASSED) {
			passed.appendHitRange(getPosition(), getPosition() + sqlPart.length() - 1);
		}
		super.passed(passed);
	}
}
