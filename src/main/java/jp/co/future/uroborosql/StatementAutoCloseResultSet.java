package jp.co.future.uroborosql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

/**
 * ResultSetのラッパークラス。ResultSetのクローズに合わせてStatementもクローズする。
 *
 * @author H.Sugimoto
 * @version 0.5.0
 */
public class StatementAutoCloseResultSet extends AbstractResultSetWrapper {
	/** 同期してクローズするStatement */
	private Statement stmt;

	/**
	 * コンストラクタ
	 *
	 * @param wrapped 元となるResultSet
	 * @param stmt Statement
	 */
	StatementAutoCloseResultSet(final ResultSet wrapped, final Statement stmt) {
		super(wrapped);
		this.stmt = stmt;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.AbstractResultSetWrapper#close()
	 */
	@Override
	public void close() throws SQLException {
		try {
			super.close();
		} finally {
			if (stmt != null && !stmt.isClosed()) {
				stmt.close();
			}
		}
	}
}
