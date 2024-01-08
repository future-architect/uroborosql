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
import jp.co.future.uroborosql.event.BeforeEndTransactionEvent;
import jp.co.future.uroborosql.event.BeforeSetParameterEvent;
import jp.co.future.uroborosql.event.SqlBatchEvent;
import jp.co.future.uroborosql.event.SqlQueryEvent;
import jp.co.future.uroborosql.event.SqlUpdateEvent;

/**
 * デバッグログを出力するEventSubscriber
 *
 * @author H.Sugimoto
 */
public class DebugEventSubscriber extends EventSubscriber {
	/** ロガー */
	private static final Logger EVENT_LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.event");

	@Override
	public void initialize() {
		afterBeginTransactionListener(this::afterBeginTransaction);
		beforeEndTransactionListener(this::beforeEndTransaction);
		beforeSetParameterListener(this::beforeSetParameter);
		afterGetOutParameterListener(this::afterGetOutParameter);
		sqlQueryListener(this::sqlQuery);
		sqlUpdateListener(this::sqlUpdate);
		sqlBatchListener(this::sqlBatch);
	}

	void afterBeginTransaction(final AfterBeginTransactionEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			try {
				EVENT_LOG.debug("Begin Transaction - connection:{}",
						evt.getTransactionContext().getConnection(), evt.occurredOn());
			} catch (SQLException e) {
				EVENT_LOG.error(e.getMessage(), e);
			}
		}
	}

	void beforeEndTransaction(final BeforeEndTransactionEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			try {
				EVENT_LOG.debug("End Transaction - connection:{}, result:{}",
						evt.getTransactionContext().getConnection(),
						evt.getResult());
			} catch (SQLException e) {
				EVENT_LOG.error(e.getMessage(), e);
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

	void sqlQuery(final SqlQueryEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			EVENT_LOG.debug("Execute Query - sqlName:{} executed.", evt.getExecutionContext().getSqlName());
			EVENT_LOG.trace("Execute Query sql:{}", evt.getPreparedStatement());
		}
	}

	void sqlUpdate(final SqlUpdateEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			EVENT_LOG.debug("Execute Update - sqlName:{} executed. Count:{} items.",
					evt.getExecutionContext().getSqlName(), evt.getCount());
		}
	}

	void sqlBatch(final SqlBatchEvent evt) {
		if (EVENT_LOG.isDebugEnabled()) {
			var counts = evt.getCounts();
			try {
				counts = new int[] { evt.getPreparedStatement().getUpdateCount() };
			} catch (SQLException ex) {
				ex.printStackTrace();
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