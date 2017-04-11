package jp.co.future.uroborosql;

import java.sql.SQLException;
import java.util.Map;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.fluent.Procedure;

/**
 * Procedure実装
 *
 * @author H.Sugimoto
 */
final class ProcedureImpl extends AbstractSqlFluent<Procedure> implements Procedure {
	private final SqlAgent agent;

	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context SqlContext
	 */
	ProcedureImpl(final SqlAgent agent, final SqlContext context) {
		super(context);
		this.agent = agent;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.Procedure#call()
	 */
	@Override
	public Map<String, Object> call() throws SQLException {
		return agent.procedure(context());
	}
}
