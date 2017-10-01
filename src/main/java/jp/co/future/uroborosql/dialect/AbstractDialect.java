package jp.co.future.uroborosql.dialect;

import java.util.Collections;
import java.util.List;

/**
 * Dialectの抽象親クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractDialect implements Dialect {
	private List<String> sqlRetryCodes = Collections.emptyList();

	@Override
	public List<String> getSqlRetryCodes() {
		return sqlRetryCodes;
	}

	@Override
	public void setSqlRetryCodes(List<String> sqlRetryCodes) {
		this.sqlRetryCodes = sqlRetryCodes;
	}
}
