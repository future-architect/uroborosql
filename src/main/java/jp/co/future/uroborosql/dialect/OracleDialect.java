package jp.co.future.uroborosql.dialect;


/**
 * Oracle用のDialect
 *
 * @author H.Sugimoto
 */
public class OracleDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public OracleDialect() {
		super();
	}

	@Override
	public String getName() {
		return "Oracle";
	}

}
