package jp.co.future.uroborosql.dialect;

import java.util.Arrays;

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
