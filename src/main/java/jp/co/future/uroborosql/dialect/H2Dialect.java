package jp.co.future.uroborosql.dialect;

import java.sql.DatabaseMetaData;
import java.sql.SQLException;

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
		return "h2";
	}

	@Override
	public String getDriverClassName() {
		return "org.h2.Driver";
	}
}
