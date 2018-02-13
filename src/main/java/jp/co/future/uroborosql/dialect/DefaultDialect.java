package jp.co.future.uroborosql.dialect;

import jp.co.future.uroborosql.connection.ConnectionSupplier;

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
	public String getDatabaseName() {
		return "default";
	}

	@Override
	public boolean accept(ConnectionSupplier supplier) {
		return true;
	}
}
