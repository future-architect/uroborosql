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
	 * @param position 開始位置
	 * @param sqlPart SQL文
	 */
	public SqlNode(final int position, final String sqlPart) {
		super(position, sqlPart.length());
		this.sqlPart = sqlPart;
	}

	/**
	 * コンストラクタ
	 *
	 * @param position 開始位置
	 * @param addLength 追加データ長
	 * @param sqlPart SQL文
	 */
	public SqlNode(final int position, final int addLength, final String sqlPart) {
		super(position, addLength + sqlPart.length());
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
}
