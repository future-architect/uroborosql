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

import jp.co.future.uroborosql.event.EventListenerHolder;
import jp.co.future.uroborosql.event.EventSubscriber;

/**
 * デバッグログを出力するEventSubscriber
 *
 * @author H.Sugimoto
 */
public class DebugEventSubscriber implements EventSubscriber {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.log.event");

	@Override
	public void subscribe(EventListenerHolder eventListenerHolder) {
		eventListenerHolder
				.addAfterBeginTransactionListeners(evt -> {
					if (LOG.isDebugEnabled()) {
						LOG.debug("Begin Transaction occuredOn:{}, connection:{}", evt.occurredOn(),
								evt.getConnection(), evt.occurredOn());
					}
				})
				.addBeforeEndTransactionListeners(evt -> {
					if (LOG.isDebugEnabled()) {
						LOG.debug("End Transaction occuredOn:{}, connection:{}, result:{}", evt.occurredOn(),
								evt.getConnection(), evt.getResult());
					}
				})
				.addBeforeSetParameterListeners(evt -> {
					if (LOG.isDebugEnabled()) {
						LOG.debug("Before Set Parameter occuredOn:{}, Parameter:{}", evt.occurredOn(),
								evt.getParameter());
					}
				})
				.addAfterGetOutParameterListeners(evt -> {
					if (LOG.isDebugEnabled()) {
						LOG.debug("After Get OutParameter occuredOn:{}, key:{}, value:{}. parameterIndex:{}",
								evt.occurredOn(), evt.getKey(), evt.getValue(), evt.getParameterIndex());
					}
				})
				.addSqlQueryListeners(evt -> {
					if (LOG.isDebugEnabled()) {
						LOG.debug("Execute Query occuredOn:{}, sqlName:{} executed.", evt.occurredOn(),
								evt.getExecutionContext().getSqlName());
						LOG.trace("Execute Query sql:{}", evt.getPreparedStatement());
					}
				})
				.addSqlUpdateListeners(evt -> {
					if (LOG.isDebugEnabled()) {
						LOG.debug("Execute Update occuredOn:{}, sqlName:{} executed. Count:{} items.", evt.occurredOn(),
								evt.getExecutionContext().getSqlName(), evt.getCount());
					}
				})
				.addSqlBatchListeners(evt -> {
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
						LOG.debug("Execute Update occuredOn:{}, sqlName:{} executed. Results:{}", evt.occurredOn(),
								evt.getExecutionContext().getSqlName(), counts);
					}
				});
	}
}