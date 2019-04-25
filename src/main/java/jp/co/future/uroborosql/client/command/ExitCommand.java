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

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.store.SqlManager;

/**
 * Exit REPL Command
 *
 * @author H.Sugimoto
 */
public class ExitCommand extends ReplCommand {

	/**
	 * Constructor
	 */
	@SuppressWarnings("unchecked")
	public ExitCommand() {
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
		Terminal terminal = reader.getTerminal();
		terminal.writer().println("SQL REPL end.");
		terminal.writer().flush();

		try {
			SqlManager sqlManager = sqlConfig.getSqlManager();
			sqlManager.shutdown();
		} catch (Exception ex) {
			// do nothing
		}
		return false;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.client.command.ReplCommand#showHelp(org.jline.terminal.Terminal)
	 */
	@Override
	public void showHelp(final Terminal terminal) {
		terminal.writer().println("\t" + this.toString() + "\t: exit SQL REPL. `CTRL+C` is an alias.");
	}
}
