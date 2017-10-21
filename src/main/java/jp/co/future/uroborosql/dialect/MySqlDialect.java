package jp.co.future.uroborosql.dialect;

/**
 * MySQL用のDialect
 *
 * @author H.Sugimoto
 */
public class MySqlDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public MySqlDialect() {
		super();
	}

	@Override
	public String getName() {
		return "MySQL";
	}

}
