package jp.co.future.uroborosql.dialect;

import java.sql.Driver;

/**
 * 標準のDialect
 *
 * @author H.Sugimoto
 */
public class DefaultDialect extends AbstractDialect {
	private String driverClassName = null;

	/**
	 * コンストラクタ
	 */
	public DefaultDialect() {
		super();
	}

	@Override
	public String getName() {
		return "default";
	}

	@Override
	public String getDriverClassName() {
		return driverClassName;
	}

	@Override
	public boolean accept(Driver driver) {
		driverClassName = driver != null ? driver.getClass().getName() : "default.driver";
		return true;
	}
}
