/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.util.Arrays;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.BiPredicate;
import java.util.stream.Stream;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.fluent.SqlBatch;
import jp.co.future.uroborosql.fluent.TriConsumer;
import jp.co.future.uroborosql.utils.BeanAccessor;

/**
 * SqlBatch実装
 *
 * @author H.Sugimoto
 * @since 0.5.0
 */
final class SqlBatchImpl extends AbstractSqlFluent<SqlBatch> implements SqlBatch {
	private static final BiPredicate<SqlContext, Map<String, Object>> DEFAULT_BATCH_WHEN_CONDITION = (ctx, row) -> ctx
			.batchCount() == 1000;

	private static final BiConsumer<SqlAgent, SqlContext> DEFAULT_BATCH_ACTION = (agent, ctx) -> {
		/* do nothing */
	};

	private static final TriConsumer<SqlAgent, SqlContext, Exception> DEFAULT_ERROR_ACTION = (agent, ctx, ex) -> {
		throw new UroborosqlRuntimeException(ex);
	};

	/** 一括更新処理用のバッチパラメータを格納したStream */
	private Stream<?> stream = null;

	/** 一括更新の発行判定条件 */
	private BiPredicate<SqlContext, Map<String, Object>> condition = DEFAULT_BATCH_WHEN_CONDITION;

	/** バッチ実行時アクション */
	private BiConsumer<SqlAgent, SqlContext> batchAction = DEFAULT_BATCH_ACTION;

	/** エラー時アクション */
	private TriConsumer<SqlAgent, SqlContext, Exception> errorAction = DEFAULT_ERROR_ACTION;

	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context SqlContext
	 */
	SqlBatchImpl(final SqlAgent agent, final SqlContext context) {
		super(agent, context);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlBatch#paramStream(java.util.stream.Stream)
	 */
	@Override
	public SqlBatch paramStream(final Stream<?> stream) {
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
	 * @see jp.co.future.uroborosql.fluent.SqlBatch#by(java.util.function.BiPredicate)
	 */
	@Override
	public SqlBatch by(final BiPredicate<SqlContext, Map<String, Object>> condition) {
		this.condition = condition;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlBatch#batchWhen(java.util.function.BiConsumer)
	 */
	@Override
	public SqlBatch batchWhen(final BiConsumer<SqlAgent, SqlContext> action) {
		this.batchAction = action;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlBatch#errorWhen(jp.co.future.uroborosql.fluent.TriConsumer)
	 */
	@Override
	public SqlBatch errorWhen(final TriConsumer<SqlAgent, SqlContext, Exception> action) {
		this.errorAction = action;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlBatch#count()
	 */
	@Override
	public int count() {
		try (Stream<?> paramStream = stream) {
			int count = paramStream.map(r -> {
				Map<String, Object> m = toMap(r);
				paramMap(m);
				context().addBatch();
				return condition.test(context(), m) ? executeBatch() : 0;
			}).reduce(0, (joined, element) -> joined + element);
			return count + (context().batchCount() != 0 ? executeBatch() : 0);
		} finally {
			stream = null;
			condition = DEFAULT_BATCH_WHEN_CONDITION;
			batchAction = DEFAULT_BATCH_ACTION;
			errorAction = DEFAULT_ERROR_ACTION;
		}
	}

	/**
	 * バッチ処理の実行
	 * @return 更新件数
	 */
	private int executeBatch() {
		try {
			return Arrays.stream(agent().batch(context())).sum();
		} catch (Exception ex) {
			errorAction.accept(agent(), context(), ex);
		} finally {
			batchAction.accept(agent(), context());
		}
		return 0;
	}

	/**
	 * ObjectをMapに変換します。
	 * @return Map
	 */
	@SuppressWarnings("unchecked")
	private Map<String, Object> toMap(final Object o) {
		if (o instanceof Map) {
			return (Map<String, Object>) o;
		}
		return BeanAccessor.asMap(o);
	}
}
