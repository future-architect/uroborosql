/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.completer;

import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.jline.reader.Candidate;
import org.jline.reader.LineReader;
import org.jline.reader.ParsedLine;

import jp.co.future.uroborosql.client.command.ReplCommand;
import jp.co.future.uroborosql.store.SqlResourceManager;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * SQL Name の補完を行うCompleter
 *
 * @author H.Sugimoto
 *
 */
public class SqlNameCompleter extends AbstractCompleter {
	private final SqlResourceManager sqlResourceManager;

	/**
	 * Constructor
	 *
	 * @param commands ReplCommand List
	 * @param sqlResourceManager sqlResourceManager
	 */
	public SqlNameCompleter(final List<ReplCommand> commands, final SqlResourceManager sqlResourceManager) {
		super(commands);
		this.sqlResourceManager = sqlResourceManager;
	}

	/**
	 * コンストラクタから渡されたSQL名の一覧と入力を比較し、前方一致するものがあれば補完対象とする<br>
	 *
	 * {@inheritDoc}
	 *
	 * @see org.jline.reader.Completer#complete(org.jline.reader.LineReader, org.jline.reader.ParsedLine, java.util.List)
	 */
	@Override
	public void complete(final LineReader reader, final ParsedLine line, final List<Candidate> candidates) {
		var buffer = line.line().substring(0, line.cursor());
		var parts = getLineParts(buffer);
		var len = parts.length;
		var complete = getLineParts(line.line()).length == len;

		// コード補完する引数の番号を特定。
		var startArgNo = getStartArgNo(line);

		// 対象引数が-1、または開始引数にlenが満たない場合は該当なしなのでコード補完しない
		if (!accept(startArgNo, buffer, len)) {
			return;
		}

		var isBlank = buffer.endsWith(" ");
		SortedSet<String> sqlNames = new TreeSet<>(sqlResourceManager.getSqlPathList());

		if (len == startArgNo && isBlank || len == startArgNo + 1 && !isBlank) {
			// コマンドが引数ありの場合
			var args = len == startArgNo + 1 ? parts[startArgNo] : "";
			if (StringUtils.isEmpty(args)) {
				candidates.addAll(sqlNames.stream().map(n -> new Candidate(n, n, null, null, null, null, complete))
						.collect(Collectors.toList()));
			} else {
				for (String match : sqlNames.tailSet(args)) {
					if (!match.startsWith(args)) {
						break;
					}
					candidates.add(new Candidate(match, match, null, null, null, null, complete));
				}
			}
		}
	}
}