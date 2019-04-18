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

/**
 * Show this message Command
 *
 * @author H.Sugimoto
 */
public class ThisCommand extends ReplCommand {

	/**
	 * Constructor
	 */
	@SuppressWarnings("unchecked")
	public ThisCommand() {
		super(true);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.client.command.ReplCommand#execute(org.jline.reader.LineReader, java.lang.String[], jp.co.future.uroborosql.config.SqlConfig, java.util.Properties)
	 */
	@Override
	public boolean execute(final LineReader reader, final String[] parts, final SqlConfig sqlConfig,
			final Properties props) {
		showMessage(reader.getTerminal(), "/this.txt");

		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.client.command.ReplCommand#showHelp(org.jline.terminal.Terminal)
	 */
	@Override
	public void showHelp(final Terminal terminal) {
		// do nothing
	}
}
