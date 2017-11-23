package jp.co.future.uroborosql;

import java.sql.SQLException;
import java.util.Map;
import java.util.function.BiPredicate;
import java.util.stream.Stream;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.SqlUpdate;

import org.apache.commons.lang3.ArrayUtils;

/**
 * SqlUpdate実装
 *
 * @author H.Sugimoto
 */
final class SqlUpdateImpl extends AbstractSqlFluent<SqlUpdate> implements SqlUpdate {
	private static final BiPredicate<SqlContext, Map<String, Object>> DEFAULT_BATCH_FRAME = (ctx, row) -> ctx
			.batchCount() == 1000;
	/** SqlAgent */
	private final SqlAgent agent;
	/** バッチ処理を行うかどうか */
	private boolean batch = false;

	/** 一括更新処理用のバッチパラメータを格納したStream */
	private Stream<Map<String, Object>> stream = null;

	/** 一括更新の発行判定条件 */
	private BiPredicate<SqlContext, Map<String, Object>> batchFrame = DEFAULT_BATCH_FRAME;

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
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#paramStream(java.util.stream.Stream)
	 */
	@Override
	public SqlUpdate paramStream(final Stream<Map<String, Object>> stream) {
		this.stream = stream;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#batchFrame(java.util.function.BiPredicate)
	 */
	@Override
	public SqlUpdate batchFrame(final BiPredicate<SqlContext, Map<String, Object>> batchFrame) {
		this.batchFrame = batchFrame;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#count()
	 */
	@Override
	public int count() {
		if (batch) {
			throw new IllegalStateException("すでにaddBatch()でパラメータが設定されているため、batch()を呼び出してください");
		}
		try {
			return agent.update(context());
		} catch (SQLException e) {
			throw new UroborosqlSQLException(e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#batch()
	 */
	@Override
	public int[] batch() {
		if (stream != null) {
			return streamBatch();
		} else {
			if (context().batchCount() == 0) {
				return new int[0];
			}
			if (!batch) {
				addBatch();
			}

			try {
				return agent.batch(context());
			} catch (SQLException e) {
				throw new UroborosqlSQLException(e);
			}
		}
	}

	/**
	 * Streamを利用した一括更新処理
	 * @return 更新件数
	 */
	private int[] streamBatch() {
		try (Stream<Map<String, Object>> paramStream = stream) {
			int[] result = paramStream.map(r -> {
				paramMap(r).addBatch();
				if (batchFrame.test(context(), r)) {
					try {
						return agent.batch(context());
					} catch (Exception e) {
						throw new UroborosqlSQLException(e);
					}
				} else {
					return null;
				}
			}).reduce(new int[0], (joined, element) -> ArrayUtils.addAll(joined, element));
			return ArrayUtils.addAll(result, context().batchCount() != 0 ? agent.batch(context()) : null);
		} catch (SQLException e) {
			throw new UroborosqlSQLException(e);
		} finally {
			stream = null;
			batchFrame = DEFAULT_BATCH_FRAME;
		}
	}

}
