package jp.co.future.uroborosql.dialect;

import jp.co.future.uroborosql.connection.ConnectionSupplier;

import java.sql.Driver;

/**
 * 標準のDialect
 *
 * @author H.Sugimoto
 */
public class DefaultDialect extends AbstractDialect {
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
	public boolean accept(ConnectionSupplier supplier) {
		return true;
	}
}
