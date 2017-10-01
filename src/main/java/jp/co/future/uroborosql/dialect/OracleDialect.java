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
		setSqlRetryCodes(Arrays.asList(
				"54", /* ORA-00054: リソース・ビジー、NOWAITが指定されていました。 */
				"60", /* ORA-00060: リソース待機の間にデッドロックが検出されました。 */
				"30006" /* ORA-30006: リソース・ビジー; WAITタイムアウトの期限に達しました。 */
		));
	}

	@Override
	public String getName() {
		return "oracle";
	}

	@Override
	public String getDriverClassName() {
		return "oracle.jdbc.OracleDriver";
	}
}
