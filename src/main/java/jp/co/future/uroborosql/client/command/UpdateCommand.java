/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.command;

import java.sql.SQLException;
import java.util.Arrays;
import java.util.Properties;

import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;

import jp.co.future.uroborosql.client.SqlParamUtils;
import jp.co.future.uroborosql.client.completer.BindParamCompleter;
import jp.co.future.uroborosql.client.completer.SqlNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;

/**
 * Execute sql update Command
 *
 * @author H.Sugimoto
 */
public class UpdateCommand extends ReplCommand {

	/**
	 * Constructor
	 */
	@SuppressWarnings("unchecked")
	public UpdateCommand() {
		super(false, SqlNameCompleter.class, BindParamCompleter.class);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.client.command.ReplCommand#execute(org.jline.reader.LineReader, java.lang.String[], jp.co.future.uroborosql.config.SqlConfig, java.util.Properties)
	 */
	@Override
	public boolean execute(final LineReader reader, final String[] parts, final SqlConfig sqlConfig,
			final Properties props) {
		var writer = reader.getTerminal().writer();
		if (parts.length >= 2) {
			var sqlName = parts[1].replace('.', '/');
			if (sqlConfig.getSqlResourceManager().existSql(sqlName)) {

				try (var agent = sqlConfig.agent()) {
					var ctx = agent.context().setSqlName(sqlName);
					ctx.setSql(sqlConfig.getSqlResourceManager().getSql(ctx.getSqlName()));
					var params = Arrays.copyOfRange(parts, 2, parts.length);
					SqlParamUtils.setSqlParams(sqlConfig, ctx, params);
					try {
						var ans = agent.update(ctx);
						agent.commit();
						writer.println("update sql[" + sqlName + "] end. row count=" + ans);
					} catch (ParameterNotFoundRuntimeException | SQLException ex) {
						writer.println("Error : " + ex.getMessage());
						agent.rollback();
					}
				}
			} else {
				writer.println("SQL not found. sql=" + sqlName);
			}
		}
		writer.flush();
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.client.command.ReplCommand#showHelp(org.jline.terminal.Terminal)
	 */
	@Override
	public void showHelp(final Terminal terminal) {
		terminal.writer().println("\t" + this.toString() + "\t: execute update from loaded sql file.");
		terminal.writer().println("\t\tex1) update [sql file name]<Enter> : Execute SQL without parameter.");
		terminal.writer().println(
				"\t\tex2) update [sql file name] param1=val1 param2=val2 ...<Enter> : Execute SQL with the specified parameters.");
	}
}
