/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.completer;

import java.util.List;
import java.util.stream.Stream;

import org.jline.reader.Candidate;
import org.jline.reader.LineReader;
import org.jline.reader.ParsedLine;

/**
 * SQLキーワードを補完するCompleter
 *
 * @author H.Sugimoto
 *
 */
public class SqlKeywordCompleter extends AbstractCompleter {
	/**
	 * {@inheritDoc}
	 *
	 * @see org.jline.reader.Completer#complete(org.jline.reader.LineReader, org.jline.reader.ParsedLine, java.util.List)
	 */
	@Override
	public void complete(final LineReader reader, final ParsedLine line, final List<Candidate> candidates) {
		String buffer = line.line();
		String[] parts = getLineParts(buffer);
		int len = parts.length;

		// コード補完する引数の番号を特定。
		int startArgNo = getStartArgNo(line);

		// 対象引数が-1、または開始引数にlenが満たない場合は該当なしなのでコード補完しない
		if (!accept(startArgNo, buffer, len)) {
			return;
		}

		String key = null;

		boolean isBlank = buffer.endsWith(" ");
		if (len == startArgNo && isBlank) {
			key = null;
		} else if (len == startArgNo + 1 && !isBlank) {
			key = parts[len - 1];
		} else {
			return;
		}

		final String keyword = key;

		Stream.of("select", "insert", "update", "delete")
				.filter(c -> keyword == null || c.startsWith(keyword.toLowerCase()))
				.forEach(c -> candidates.add(new Candidate(c)));
	}
}