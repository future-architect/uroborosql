package jp.co.future.uroborosql.node;

import jp.co.future.uroborosql.parser.TransformContext;

/**
 * SQL文を表すノード. 前にAND または ORがある場合に利用される
 *
 * @author H.Sugimoto
 */
public class PrefixSqlNode extends SqlNode {

	/** プレフィックス "AND" または "OR"のいづれかが設定される */
	private final String prefix;

	/**
	 * コンストラクタ
	 *
	 * @param position 開始位置
	 * @param prefix プレフィックス
	 * @param sqlPart SQL文
	 */
	public PrefixSqlNode(final int position, final String prefix, final String sqlPart) {
		super(position, prefix.length(), sqlPart);
		this.prefix = prefix;
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
		super.accept(transformContext);
	}
}
