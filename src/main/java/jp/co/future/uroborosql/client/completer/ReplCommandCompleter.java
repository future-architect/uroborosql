/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.completer;

import java.util.List;

import org.jline.reader.Candidate;
import org.jline.reader.Completer;
import org.jline.reader.LineReader;
import org.jline.reader.ParsedLine;

import jp.co.future.uroborosql.client.command.ReplCommand;

/**
 * REPLコマンドを補完するCompleter
 *
 * @author H.Sugimoto
 */
public class ReplCommandCompleter implements Completer {
	private final List<ReplCommand> commands;

	/**
	 * Constructor
	 * @param commands ReplCommand List
	 */
	public ReplCommandCompleter(final List<ReplCommand> commands) {
		this.commands = commands;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.jline.reader.Completer#complete(org.jline.reader.LineReader, org.jline.reader.ParsedLine, java.util.List)
	 */
	@Override
	public void complete(final LineReader reader, final ParsedLine line, final List<Candidate> candidates) {
		if (line.wordIndex() == 0) {
			commands.stream().filter(c -> c.match(line.word()))
					.map(c -> new Candidate(c.toString()))
					.forEach(candidates::add);
		}
	}
}