package jp.co.future.uroborosql;

import java.sql.SQLException;
import java.util.Map;
import java.util.function.BiPredicate;
import java.util.stream.Stream;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.CommitTiming;
import jp.co.future.uroborosql.fluent.ErrorAction;
import jp.co.future.uroborosql.fluent.SqlUpdate;

import org.apache.commons.lang3.ArrayUtils;

/**
 * SqlUpdate実装
 *
 * @author H.Sugimoto
 */
final class SqlUpdateImpl extends AbstractSqlFluent<SqlUpdate> implements SqlUpdate {
	private static final BiPredicate<SqlContext, Map<String, Object>> DEFAULT_BATCH_WHEN_CONDITION = (ctx, row) -> ctx
			.batchCount() == 1000;
	/** SqlAgent */
	private final SqlAgent agent;
	/** バッチ処理を行うかどうか */
	private boolean batch = false;

	/** 一括更新処理用のバッチパラメータを格納したStream */
	private Stream<Map<String, Object>> stream = null;

	/** 一括更新の発行判定条件 */
	private BiPredicate<SqlContext, Map<String, Object>> condition = DEFAULT_BATCH_WHEN_CONDITION;

	/** コミット実行タイミング */
	private CommitTiming commitTiming = CommitTiming.DO_NOTHING;

	/** エラー時アクション */
	private ErrorAction errorAction = ErrorAction.THROW_EXCEPTION;

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
		if (this.stream == null) {
			this.stream = stream;
		} else {
			this.stream = Stream.concat(this.stream, stream);
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#batchWhen(java.util.function.BiPredicate)
	 */
	@Override
	public SqlUpdate batchWhen(final BiPredicate<SqlContext, Map<String, Object>> condition) {
		this.condition = condition;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#batchCommitWhen(jp.co.future.uroborosql.fluent.SqlUpdate.CommitTiming)
	 */
	@Override
	public SqlUpdate batchCommitWhen(final CommitTiming commitTiming) {
		this.commitTiming = commitTiming;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#batchErrorWhen(jp.co.future.uroborosql.fluent.SqlUpdate.ErrorAction)
	 */
	@Override
	public SqlUpdate batchErrorWhen(final ErrorAction errorAction) {
		this.errorAction = errorAction;
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
			return executeBatch();
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
				return condition.test(context(), r) ? executeBatch() : null;
			}).reduce(new int[0], (joined, element) -> element != null ? ArrayUtils.addAll(joined, element) : joined);
			return ArrayUtils.addAll(result, context().batchCount() != 0 ? executeBatch() : null);
		} finally {
			stream = null;
			condition = DEFAULT_BATCH_WHEN_CONDITION;
			commitTiming = CommitTiming.DO_NOTHING;
			errorAction = ErrorAction.THROW_EXCEPTION;
		}
	}

	/**
	 * バッチ処理の実行
	 * @return 更新件数
	 */
	private int[] executeBatch() {
		try {
			return agent.batch(context());
		} catch (Exception e) {
			if (ErrorAction.THROW_EXCEPTION.equals(errorAction)) {
				throw new UroborosqlSQLException(e);
			}
		} finally {
			if (CommitTiming.COMMIT_EACH_TIME.equals(commitTiming)) {
				agent.commit();
			}
		}
		return new int[0];
	}

}
