package jp.co.future.uroborosql.event;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * SQL変換前イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class BeforeTransformSqlEvent extends ExecutionEvent {
	/** 変換前SQL. */
	private String sql;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param sql 変換前SQL
	 */
	public BeforeTransformSqlEvent(final ExecutionContext executionContext, final String sql) {
		super(executionContext);
		this.sql = sql;
	}

	/**
	 * 変換前SQLの取得.
	 * @return 変換前SQL
	 */
	public String getSql() {
		return sql;
	}

	/**
	 * 変換前SQLの設定.
	 * @param sql 変換前SQL
	 */
	public void setSql(final String sql) {
		this.sql = sql;
	}
}
