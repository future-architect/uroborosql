/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event.subscriber;

import java.sql.SQLException;

import org.slf4j.Logger;

import jp.co.future.uroborosql.event.AfterBeginTransactionEvent;
import jp.co.future.uroborosql.event.AfterGetOutParameterEvent;
import jp.co.future.uroborosql.event.AfterSqlBatchEvent;
import jp.co.future.uroborosql.event.AfterSqlQueryEvent;
import jp.co.future.uroborosql.event.AfterSqlUpdateEvent;
import jp.co.future.uroborosql.event.BeforeEndTransactionEvent;
import jp.co.future.uroborosql.event.BeforeSetParameterEvent;
import jp.co.future.uroborosql.log.EventLogger;

/**
 * デバッグログを出力するEventSubscriber
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class DebugEventSubscriber extends EventSubscriber {
	/** ロガー */
	private static final Logger EVENT_LOG = EventLogger.getEventLogger("debug");

	@Override
	public void initialize() {
		afterBeginTransactionListener(this::afterBeginTransaction);
		beforeEndTransactionListener(this::beforeEndTransaction);
		beforeSetParameterListener(this::beforeSetParameter);
		afterGetOutParameterListener(this::afterGetOutParameter);
		afterSqlQueryListener(this::afterSqlQuery);
		afterSqlUpdateListener(this::afterSqlUpdate);
		afterSqlBatchListener(this::afterSqlBatch);
	}

	void afterBeginTransaction(final AfterBeginTransactionEvent evt) {
		try {
			EVENT_LOG.atDebug()
					.setMessage("Begin Transaction - connection:{}, requiredNew:{}, transactionLevel:{}, occurredOn:{}")
					.addArgument(evt.getTransactionContext().getConnection())
					.addArgument(evt.isRequiredNew())
					.addArgument(evt.getTransactionLevel())
					.addArgument(evt.occurredOn())
					.log();
		} catch (SQLException ex) {
			EVENT_LOG.atError()
					.setMessage(ex.getMessage())
					.setCause(ex)
					.log();
		}
	}

	void beforeEndTransaction(final BeforeEndTransactionEvent evt) {
		try {
			EVENT_LOG.atDebug()
					.setMessage(
							"End Transaction - connection:{}, requiredNew:{}, transactionLevel:{}, result:{}, occurredOn:{}")
					.addArgument(evt.getTransactionContext().getConnection())
					.addArgument(evt.isRequiredNew())
					.addArgument(evt.getTransactionLevel())
					.addArgument(evt.getResult())
					.addArgument(evt.occurredOn())
					.log();
		} catch (SQLException ex) {
			EVENT_LOG.atError()
					.setMessage(ex.getMessage())
					.setCause(ex)
					.log();
		}
	}

	void beforeSetParameter(final BeforeSetParameterEvent evt) {
		EVENT_LOG.atDebug()
				.setMessage("Before Set Parameter - Parameter:{}")
				.addArgument(evt.getParameter())
				.log();
	}

	void afterGetOutParameter(final AfterGetOutParameterEvent evt) {
		EVENT_LOG.atDebug()
				.setMessage("After Get OutParameter - key:{}, value:{}. parameterIndex:{}")
				.addArgument(evt.getKey())
				.addArgument(evt.getValue())
				.addArgument(evt.getParameterIndex())
				.log();
	}

	void afterSqlQuery(final AfterSqlQueryEvent evt) {
		EVENT_LOG.atDebug()
				.setMessage("Execute Query - sqlName:{} executed.")
				.addArgument(evt.getExecutionContext().getSqlName())
				.log();
		EVENT_LOG.atTrace()
				.setMessage("Execute Query sql:{}")
				.addArgument(evt.getPreparedStatement())
				.log();
	}

	void afterSqlUpdate(final AfterSqlUpdateEvent evt) {
		EVENT_LOG.atDebug()
				.setMessage("Execute Update - sqlName:{} executed. Count:{} items.")
				.addArgument(evt.getExecutionContext().getSqlName())
				.addArgument(evt.getCount())
				.log();
	}

	void afterSqlBatch(final AfterSqlBatchEvent evt) {
		EVENT_LOG.atDebug()
				.setMessage("Execute Update - sqlName:{} executed. Results:{}")
				.addArgument(evt.getExecutionContext().getSqlName())
				.addArgument(() -> {
					try {
						return new int[] { evt.getPreparedStatement().getUpdateCount() };
					} catch (SQLException ex) {
						EVENT_LOG.atError()
								.setMessage(ex.getMessage())
								.setCause(ex)
								.log();
						return evt.getCounts();
					}
				})
				.log();
	}
}