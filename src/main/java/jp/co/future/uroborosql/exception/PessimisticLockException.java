package jp.co.future.uroborosql.exception;

import java.sql.SQLException;

import jp.co.future.uroborosql.context.SqlContext;

/**
 * 悲観的ロック例外
 *
 * @author H.Sugimoto
 * @since v0.6.0
 */
public class PessimisticLockException extends SQLException {
	private int retryCount;

	/**
	 * コンストラクタ
	 *
	 * @param context 実行したSqlContext
	 * @param retryCount リトライカウント
	 * @param cause 発生したSQLException
	 */
	public PessimisticLockException(final SqlContext context, final int retryCount, final SQLException cause) {
		super(String.format(
				"An error occurred due to pessimistic locking.\nExecuted SQL [\n%s]\nparams:%s\nretry count:%d",
				context.getExecutableSql(), context.formatParams(), retryCount)
				, cause.getSQLState(), cause.getErrorCode(), cause);
		this.retryCount = retryCount;
	}

	/**
	 * retryCount を取得します。
	 *
	 * @return retryCount
	 */
	public int getRetryCount() {
		return retryCount;
	}

}
