package jp.co.future.uroborosql.dialect;

/**
 * Microsoft SQLServer用のDialect
 *
 * @author H.Sugimoto
 */
public class MsSqlDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public MsSqlDialect() {
		super();
	}

	@Override
	public String getName() {
		return "Microsoft SQL Server";
	}

	/**
	 * MSSQLではMerge文で;を使用するため終端文字の削除を行わない
	 * @return false
	 */
	@Override
	public boolean isRemoveTerminator() {
		return false;
	}
}
