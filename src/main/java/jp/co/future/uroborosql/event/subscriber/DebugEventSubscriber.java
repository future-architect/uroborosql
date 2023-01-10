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

/**
 * デバッグログを出力するEventSubscriber
 *
 * @author H.Sugimoto
 */
public class DebugEventSubscriber extends EventSubscriber {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.log.event");

	@Override
	public void initialize() {
		afterBeginTransactionListener(evt -> {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Begin Transaction - connection:{}", evt.getConnection(), evt.occurredOn());
			}
		});
		beforeEndTransactionListener(evt -> {
			if (LOG.isDebugEnabled()) {
				LOG.debug("End Transaction - connection:{}, result:{}", evt.getConnection(), evt.getResult());
			}
		});
		beforeSetParameterListener(evt -> {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Before Set Parameter - Parameter:{}", evt.getParameter());
			}
		});
		afterGetOutParameterListener(evt -> {
			if (LOG.isDebugEnabled()) {
				LOG.debug("After Get OutParameter - key:{}, value:{}. parameterIndex:{}",
						evt.getKey(), evt.getValue(), evt.getParameterIndex());
			}
		});
		sqlQueryListener(evt -> {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute Query - sqlName:{} executed.", evt.getExecutionContext().getSqlName());
				LOG.trace("Execute Query sql:{}", evt.getPreparedStatement());
			}
		});
		sqlUpdateListener(evt -> {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Execute Update - sqlName:{} executed. Count:{} items.",
						evt.getExecutionContext().getSqlName(), evt.getCount());
			}
		});
		sqlBatchListener(evt -> {
			if (LOG.isDebugEnabled()) {
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
				LOG.debug("Execute Update - sqlName:{} executed. Results:{}",
						evt.getExecutionContext().getSqlName(), counts);
			}
		});
	}
}