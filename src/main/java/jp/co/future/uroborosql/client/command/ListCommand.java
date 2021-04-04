/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.command;

import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;

import jp.co.future.uroborosql.client.completer.SqlNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;

/**
 * Show sqlname list Command
 *
 * @author H.Sugimoto
 */
public class ListCommand extends ReplCommand {

	/**
	 * Constructor
	 */
	@SuppressWarnings("unchecked")
	public ListCommand() {
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
		writer.println("LIST:");
		writer.flush();

		List<String> pathList = null;
		if (parts.length > 1) {
			pathList = sqlConfig.getSqlManager().getSqlPathList().stream().filter(p -> p.contains(parts[1]))
					.collect(Collectors.toList());
		} else {
			pathList = sqlConfig.getSqlManager().getSqlPathList();
		}
		for (String key : pathList) {
			writer.println(key);
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
		terminal.writer().println("\t" + this.toString() + "\t: list loaded sql files.");
		terminal.writer().println("\t\tex1) list<Enter> : Show all loaded sql file(s).");
		terminal.writer().println("\t\tex2) list keyword<Enter> : Show loaded sql file(s) filter by keyword.");
	}
}
