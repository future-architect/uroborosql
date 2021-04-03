/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.command;

import java.util.Properties;

import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;

import jp.co.future.uroborosql.client.completer.SqlNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;

/**
 * View sql file Command
 *
 * @author H.Sugimoto
 */
public class ViewCommand extends ReplCommand {

	/**
	 * Constructor
	 */
	@SuppressWarnings("unchecked")
	public ViewCommand() {
		super(false, SqlNameCompleter.class);
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
			var sqlName = parts[1].replaceAll("\\.", "/");
			if (sqlConfig.getSqlManager().existSql(sqlName)) {
				var sql = sqlConfig.getSqlManager().getSql(sqlName);
				var sqlLines = sql.split("\\r\\n|\\r|\\n");
				for (String sqlLine : sqlLines) {
					writer.println(sqlLine);
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
		terminal.writer().println("\t" + this.toString() + "\t: view sql file.");
		terminal.writer().println("\t\tex) view [sql file name]<Enter> : Show sql file contents.");
	}
}
