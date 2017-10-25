package jp.co.future.uroborosql.dialect;

/**
 * Postgresql用のDialect
 *
 * @author H.Sugimoto
 */
public class PostgresqlDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public PostgresqlDialect() {
		super();
	}

	@Override
	public String getName() {
		return "PostgreSQL";
	}

}
