package jp.co.future.uroborosql;

import java.sql.SQLException;
import java.util.Map;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.Procedure;

/**
 * Procedure実装
 *
 * @author H.Sugimoto
 */
final class ProcedureImpl extends AbstractSqlFluent<Procedure> implements Procedure {
	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context SqlContext
	 */
	ProcedureImpl(final SqlAgent agent, final SqlContext context) {
		super(agent, context);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.Procedure#call()
	 */
	@Override
	public Map<String, Object> call() {
		try {
			return agent().procedure(context());
		} catch (SQLException e) {
			throw new UroborosqlSQLException(e);
		}
	}
}
