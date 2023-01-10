package jp.co.future.uroborosql.event;

import java.sql.PreparedStatement;
import java.sql.ResultSet;

import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * SqlQuery実行後イベントオブジェクト
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class SqlQueryEvent extends ExecutionEvent {
	/** ResultSet. */
	private ResultSet resultSet;
	/** PreparedStatement. */
	private final PreparedStatement preparedStatement;

	/**
	 * コンストラクタ.
	 *
	 * @param executionContext ExecutionContext
	 * @param resultSet ResultSet
	 * @param preparedStatement PreparedStatement
	 */
	public SqlQueryEvent(final ExecutionContext executionContext,
			final ResultSet resultSet,
			final PreparedStatement preparedStatement) {
		super(executionContext);
		this.resultSet = resultSet;
		this.preparedStatement = preparedStatement;
	}

	/**
	 * ResultSetの取得.
	 * @return ResultSet
	 */
	public ResultSet getResultSet() {
		return resultSet;
	}

	/**
	 * ResultSetの設定.
	 * @param resultSet ResultSet
	 */
	public void setResultSet(final ResultSet resultSet) {
		this.resultSet = resultSet;
	}

	/**
	 * PreparedStatementの取得.
	 * @return PreparedStatement
	 */
	public PreparedStatement getPreparedStatement() {
		return preparedStatement;
	}

}
