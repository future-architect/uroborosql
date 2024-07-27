/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event.subscriber;

import java.sql.SQLException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.event.AfterBeginTransactionEvent;
import jp.co.future.uroborosql.event.AfterGetOutParameterEvent;
import jp.co.future.uroborosql.event.AfterSqlBatchEvent;
import jp.co.future.uroborosql.event.AfterSqlQueryEvent;
import jp.co.future.uroborosql.event.AfterSqlUpdateEvent;
import jp.co.future.uroborosql.event.BeforeEndTransactionEvent;
import jp.co.future.uroborosql.event.BeforeSetParameterEvent;

/**
 * デバッグログを出力するEventSubscriber
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class DebugEventSubscriber extends EventSubscriber {
	/** ロガー */
	private static final Logger EVENT_LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.event.debug");

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
		if (EVENT_LOG.isDebugEnabled()) {
			try {
				EVENT_LOG.debug("Begin Transaction - connection:{}, requiredNew:{}, transactionLevel:{}, occurredOn:{}",
						evt.getTransactionContext().getConnection(),
						evt.isRequiredNew(),
						evt.getTransactionLevel(),
						evt.occurredOn());
			} catch (SQLException ex) {
				EVENT_LOG.error(ex.getMessage(), ex);
			}
		}
	}

	void beforeEndTransaction(final BeforeEndTransactionEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			try {
				EVENT_LOG.debug(
						"End Transaction - connection:{}, requiredNew:{}, transactionLevel:{}, result:{}, occurredOn:{}",
						evt.getTransactionContext().getConnection(),
						evt.isRequiredNew(),
						evt.getTransactionLevel(),
						evt.getResult(),
						evt.occurredOn());
			} catch (SQLException ex) {
				EVENT_LOG.error(ex.getMessage(), ex);
			}
		}
	}

	void beforeSetParameter(final BeforeSetParameterEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			EVENT_LOG.debug("Before Set Parameter - Parameter:{}", evt.getParameter());
		}
	}

	void afterGetOutParameter(final AfterGetOutParameterEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			EVENT_LOG.debug("After Get OutParameter - key:{}, value:{}. parameterIndex:{}",
					evt.getKey(), evt.getValue(), evt.getParameterIndex());
		}
	}

	void afterSqlQuery(final AfterSqlQueryEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			EVENT_LOG.debug("Execute Query - sqlName:{} executed.", evt.getExecutionContext().getSqlName());
			EVENT_LOG.trace("Execute Query sql:{}", evt.getPreparedStatement());
		}
	}

	void afterSqlUpdate(final AfterSqlUpdateEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			EVENT_LOG.debug("Execute Update - sqlName:{} executed. Count:{} items.",
					evt.getExecutionContext().getSqlName(), evt.getCount());
		}
	}

	void afterSqlBatch(final AfterSqlBatchEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			var counts = evt.getCounts();
			try {
				counts = new int[] { evt.getPreparedStatement().getUpdateCount() };
			} catch (SQLException ex) {
				EVENT_LOG.error(ex.getMessage(), ex);
			}

			var builder = new StringBuilder();
			for (int val : counts) {
				builder.append(val).append(", ");
			}
			EVENT_LOG.debug("Execute Update - sqlName:{} executed. Results:{}",
					evt.getExecutionContext().getSqlName(), counts);
		}
	}
}