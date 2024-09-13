/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event.subscriber;

import java.util.stream.Collectors;

import org.slf4j.Logger;

import jp.co.future.uroborosql.client.SqlParamUtils;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.event.AfterSqlQueryEvent;
import jp.co.future.uroborosql.event.AfterSqlUpdateEvent;
import jp.co.future.uroborosql.log.support.EventLoggingSupport;

/**
 * SQL実行結果を再現するREPLコマンドのログを出力するEventSubscriber
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class ReplCommandLogEventSubscriber extends EventSubscriber implements EventLoggingSupport {
	/** ロガー */
	private static final Logger EVENT_LOG = EventLoggingSupport.getEventLogger("replcommand");

	@Override
	public void initialize() {
		afterSqlQueryListener(this::afterSqlQuery);
		afterSqlUpdateListener(this::afterSqlUpdate);
	}

	void afterSqlQuery(final AfterSqlQueryEvent evt) {
		outputReplLog(evt.getExecutionContext());
	}

	void afterSqlUpdate(final AfterSqlUpdateEvent evt) {
		outputReplLog(evt.getExecutionContext());
	}

	/**
	 * REPLでSQLを実行するためのコマンドをログとしてREPL_LOGに出力する.
	 *
	 * @param executionContext executionContext
	 */
	private void outputReplLog(final ExecutionContext executionContext) {
		if (!EVENT_LOG.isInfoEnabled() || executionContext.getSqlName() == null
				|| (!SqlKind.SELECT.equals(executionContext.getSqlKind())
						&& !SqlKind.UPDATE.equals(executionContext.getSqlKind()))) {
			// REPLログ出力対象でない場合は何もしない
			return;
		}

		var builder = new StringBuilder();

		if (SqlKind.SELECT.equals(executionContext.getSqlKind())) {
			builder.append("query ");
		} else {
			builder.append("update ");
		}

		builder.append(executionContext.getSqlName());

		var params = executionContext.getBindNames().stream()
				.map(bindName -> executionContext.getParam(bindName))
				.collect(Collectors.toList());
		if (!params.isEmpty()) {
			builder.append(" ");
			builder.append(SqlParamUtils.formatPrams(params));
		}
		infoWith(EVENT_LOG)
				.setMessage("REPL command: {}")
				.addArgument(builder.toString())
				.log();
	}

}