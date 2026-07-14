/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event.subscriber;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Objects;

import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Tag;
import io.micrometer.core.instrument.Tags;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.event.AfterProcedureEvent;
import jp.co.future.uroborosql.event.AfterSqlBatchEvent;
import jp.co.future.uroborosql.event.AfterSqlQueryEvent;
import jp.co.future.uroborosql.event.AfterSqlUpdateEvent;

/**
 * Micrometerを使用してSQLメトリクスを収集するイベントサブスクライバ
 *
 * @author H.Sugimoto
 * @since v1.0.9
 */
public class MicrometerEventSubscriber extends EventSubscriber {
	/** MeterRegistry */
	private final MeterRegistry meterRegistry;

	/** SQL名をタグに含めるかどうか */
	private boolean includeSqlNameTag = false;

	/** SQL-IDをタグに含めるかどうか */
	private boolean includeSqlIdTag = false;

	/** 処理行数を計測するかどうか */
	private boolean includeRowCount = true;

	/**
	 * コンストラクタ
	 *
	 * @param meterRegistry MeterRegistry
	 */
	public MicrometerEventSubscriber(final MeterRegistry meterRegistry) {
		this.meterRegistry = Objects.requireNonNull(meterRegistry, "meterRegistry");
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.subscriber.EventSubscriber#initialize()
	 */
	@Override
	public void initialize() {
		afterSqlQueryListener(this::afterSqlQuery);
		afterSqlUpdateListener(this::afterSqlUpdate);
		afterSqlBatchListener(this::afterSqlBatch);
		afterProcedureListener(this::afterProcedure);
	}

	/**
	 * Query実行後の処理
	 *
	 * @param evt AfterSqlQueryEvent
	 */
	void afterSqlQuery(final AfterSqlQueryEvent evt) {
		try {
			var executionContext = evt.getExecutionContext();
			var tags = createTags(executionContext);

			// 実行回数カウンター
			meterRegistry.counter("uroborosql.sql.executions", tags).increment();

			// 実行時間タイマー
			var executionTime = executionContext.getExecutionTime();
			if (executionTime != null) {
				meterRegistry.timer("uroborosql.sql.duration", tags).record(executionTime);
			}

			// 処理行数
			if (includeRowCount) {
				var rowCount = getRowCount(evt.getResultSet());
				if (rowCount >= 0) {
					meterRegistry.summary("uroborosql.sql.rows", tags).record(rowCount);
				}
			}
		} catch (Exception ex) {
			// メトリクス記録の失敗がSQL実行に影響を与えないよう握りつぶす
		}
	}

	/**
	 * Update実行後の処理
	 *
	 * @param evt AfterSqlUpdateEvent
	 */
	void afterSqlUpdate(final AfterSqlUpdateEvent evt) {
		try {
			var executionContext = evt.getExecutionContext();
			var tags = createTags(executionContext);

			// 実行回数カウンター
			meterRegistry.counter("uroborosql.sql.executions", tags).increment();

			// 実行時間タイマー
			var executionTime = executionContext.getExecutionTime();
			if (executionTime != null) {
				meterRegistry.timer("uroborosql.sql.duration", tags).record(executionTime);
			}

			// 処理行数
			if (includeRowCount) {
				var rowCount = evt.getCount();
				meterRegistry.summary("uroborosql.sql.rows", tags).record(rowCount);
			}
		} catch (Exception ex) {
			// メトリクス記録の失敗がSQL実行に影響を与えないよう握りつぶす
		}
	}

	/**
	 * Batch実行後の処理
	 *
	 * @param evt AfterSqlBatchEvent
	 */
	void afterSqlBatch(final AfterSqlBatchEvent evt) {
		try {
			var executionContext = evt.getExecutionContext();
			var tags = createTags(executionContext);

			// 実行回数カウンター
			meterRegistry.counter("uroborosql.sql.executions", tags).increment();

			// 実行時間タイマー
			var executionTime = executionContext.getExecutionTime();
			if (executionTime != null) {
				meterRegistry.timer("uroborosql.sql.duration", tags).record(executionTime);
			}

			// 処理行数
			if (includeRowCount) {
				var counts = evt.getCounts();
				if (counts != null && counts.length > 0) {
					var totalCount = 0;
					for (var count : counts) {
						totalCount += count;
					}
					meterRegistry.summary("uroborosql.sql.rows", tags).record(totalCount);
				}
			}
		} catch (Exception ex) {
			// メトリクス記録の失敗がSQL実行に影響を与えないよう握りつぶす
		}
	}

	/**
	 * Procedure実行後の処理
	 *
	 * @param evt AfterProcedureEvent
	 */
	void afterProcedure(final AfterProcedureEvent evt) {
		try {
			var executionContext = evt.getExecutionContext();
			var tags = createTags(executionContext);

			// 実行回数カウンター
			meterRegistry.counter("uroborosql.sql.executions", tags).increment();

			// 実行時間タイマー
			var executionTime = executionContext.getExecutionTime();
			if (executionTime != null) {
				meterRegistry.timer("uroborosql.sql.duration", tags).record(executionTime);
			}
		} catch (Exception ex) {
			// メトリクス記録の失敗がSQL実行に影響を与えないよう握りつぶす
		}
	}

	/**
	 * メトリクス用のタグを作成する
	 *
	 * @param executionContext ExecutionContext
	 * @return Tags
	 */
	private Tags createTags(final ExecutionContext executionContext) {
		var tags = Tags.of(Tag.of("sql.kind", getSqlKindName(executionContext.getSqlKind())));

		if (includeSqlNameTag && executionContext.getSqlName() != null) {
			tags = tags.and(Tag.of("sql.name", executionContext.getSqlName()));
		}

		if (includeSqlIdTag && executionContext.getSqlId() != null) {
			tags = tags.and(Tag.of("sql.id", executionContext.getSqlId()));
		}

		return tags;
	}

	/**
	 * SqlKindから文字列を取得する
	 *
	 * @param sqlKind SqlKind
	 * @return SqlKindの文字列表現
	 */
	private String getSqlKindName(final SqlKind sqlKind) {
		return sqlKind != null ? sqlKind.name() : "UNKNOWN";
	}

	/**
	 * ResultSetから行数を取得する
	 *
	 * @param resultSet ResultSet
	 * @return 行数. 取得できない場合は-1
	 */
	private int getRowCount(final ResultSet resultSet) {
		var rowCount = -1;
		try {
			// resultSetのカーソル種別を取得
			// 種別「TYPE_FORWARD_ONLY」の場合、beforeFirstメソッドが効かないため除外
			if (resultSet.getType() != ResultSet.TYPE_FORWARD_ONLY) {
				// 件数結果取得
				resultSet.last();
				rowCount = resultSet.getRow();
				resultSet.beforeFirst();
			}
		} catch (SQLException ex) {
			// ここでの例外は実処理に影響を及ぼさないよう握りつぶす
		}
		return rowCount;
	}

	/**
	 * SQL名をタグに含めるかどうかを設定する
	 *
	 * @param includeSqlNameTag SQL名をタグに含めるかどうか
	 * @return MicrometerEventSubscriber
	 */
	public MicrometerEventSubscriber setIncludeSqlNameTag(final boolean includeSqlNameTag) {
		this.includeSqlNameTag = includeSqlNameTag;
		return this;
	}

	/**
	 * SQL-IDをタグに含めるかどうかを設定する
	 *
	 * @param includeSqlIdTag SQL-IDをタグに含めるかどうか
	 * @return MicrometerEventSubscriber
	 */
	public MicrometerEventSubscriber setIncludeSqlIdTag(final boolean includeSqlIdTag) {
		this.includeSqlIdTag = includeSqlIdTag;
		return this;
	}

	/**
	 * 処理行数を計測するかどうかを設定する
	 *
	 * @param includeRowCount 処理行数を計測するかどうか
	 * @return MicrometerEventSubscriber
	 */
	public MicrometerEventSubscriber setIncludeRowCount(final boolean includeRowCount) {
		this.includeRowCount = includeRowCount;
		return this;
	}
}
