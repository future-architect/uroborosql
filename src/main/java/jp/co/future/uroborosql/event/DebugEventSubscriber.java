/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.event.ResultEvent.BatchResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.QueryResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.UpdateResultEvent;
import jp.co.future.uroborosql.event.SqlEvent.OutParameterEvent;
import jp.co.future.uroborosql.event.SqlEvent.ParameterEvent;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * デバッグログを出力するイベントサブスクライバ.
 *
 * @author yanagihara
 */
public class DebugEventSubscriber implements EventSubscriber {

	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(DebugEventSubscriber.class);

	@Override
	public Parameter doParameter(final ParameterEvent event) {
		LOG.debug("Parameter:{}", event.getParameter());
		return event.getParameter();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doOutParameter(OutParameterEvent)
	 */
	@Override
	public Object doOutParameter(final OutParameterEvent event) {
		LOG.debug("Out parameter - Key:{}, value:{}", event.getKey(), event.getValue());
		return event.getValue();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doQuery(QueryResultEvent)
	 */
	@Override
	public ResultSet doQuery(final QueryResultEvent event) {
		LOG.debug("SQL:{} executed.", event.getExecutionContext().getSqlName());
		return event.getResultSet();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doUpdate(UpdateResultEvent)
	 */
	@Override
	public int doUpdate(final UpdateResultEvent event) {
		LOG.debug("SQL:{} executed. Count:{} items.", event.getExecutionContext().getSqlName(), event.getResult());
		return event.getResult();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.EventSubscriber#doBatch(BatchResultEvent)
	 */
	@Override
	public int[] doBatch(final BatchResultEvent event) {
		if (LOG.isDebugEnabled()) {
			var counts = event.getResult();
			try {
				counts = new int[] { event.getPreparedStatement().getUpdateCount() };
			} catch (SQLException ex) {
				ex.printStackTrace();
			}

			var builder = new StringBuilder();
			for (int val : counts) {
				builder.append(val).append(", ");
			}
			LOG.debug("SQL:{} executed. Result:{}", event.getExecutionContext().getSqlName(), builder.toString());
		}
		return event.getResult();
	}
}
