package jp.co.future.uroborosql.dialect;


/**
 * H2用のDialect
 *
 * @author H.Sugimoto
 */
public class H2Dialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public H2Dialect() {
		super();
	}

	@Override
	public String getName() {
		return "H2";
	}
}
