/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.command;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;

import jp.co.future.uroborosql.config.SqlConfig;

/**
 * Show command history Command
 *
 * @author H.Sugimoto
 */
public class HistoryCommand extends ReplCommand {

	/**
	 * Constructor
	 */
	@SuppressWarnings("unchecked")
	public HistoryCommand() {
		super(false);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.client.command.ReplCommand#execute(org.jline.reader.LineReader, java.lang.String[], jp.co.future.uroborosql.config.SqlConfig, java.util.Properties)
	 */
	@Override
	public boolean execute(final LineReader reader, final String[] parts, final SqlConfig sqlConfig,
			final Properties props) {
		PrintWriter writer = reader.getTerminal().writer();
		writer.println("HISTORY:");
		writer.flush();

		List<String> keywords = new ArrayList<>();
		if (parts.length > 1) {
			keywords.addAll(Arrays.asList(Arrays.copyOfRange(parts, 1, parts.length)));
		}

		int sizeLen = String.valueOf(reader.getHistory().size()).length();
		reader.getHistory().forEach(entry -> {
			try {
				String value = entry.line();
				if (keywords.isEmpty() || keywords.stream().anyMatch(s -> value.contains(s))) {
					writer.println(String.format("%" + sizeLen + "d : %s", entry.index() + 1, value));
				}
			} catch (Exception e) {
				// do nothing
			}
		});
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
		terminal.writer().println("\t" + this.toString() + "\t: list command history.");
		terminal.writer().println("\t\tex1) history<Enter> : Show all command history.");
		terminal.writer().println("\t\tex2) history keyword<Enter> : Show command history filter by keyword.");
	}
}
