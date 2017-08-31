package jp.co.future.uroborosql;

import java.sql.SQLException;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.fluent.SqlUpdate;

/**
 * SqlUpdate実装
 *
 * @author H.Sugimoto
 */
final class SqlUpdateImpl extends AbstractSqlFluent<SqlUpdate> implements SqlUpdate {
	private final SqlAgent agent;
	/** バッチ処理を行うかどうか */
	private boolean batch = false;

	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context SqlContext
	 */
	SqlUpdateImpl(final SqlAgent agent, final SqlContext context) {
		super(context);
		this.agent = agent;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#addBatch()
	 */
	@Override
	public SqlUpdate addBatch() {
		context().addBatch();
		batch = true;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#count()
	 */
	@Override
	public int count() throws SQLException {
		if (batch) {
			throw new IllegalStateException("すでにaddBatch()でパラメータが設定されているため、batch()を呼び出してください");
		}
		return agent.update(context());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#batch()
	 */
	@Override
	public int[] batch() throws SQLException {
		if (context.batchCount() == 0) {
			return new int[0];
		}
		if (!batch) {
			addBatch();
		}
		return agent.batch(context());
	}
}
