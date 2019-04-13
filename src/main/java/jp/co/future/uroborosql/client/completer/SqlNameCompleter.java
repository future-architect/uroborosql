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

import org.apache.commons.lang3.StringUtils;
import org.jline.reader.Candidate;
import org.jline.reader.LineReader;
import org.jline.reader.ParsedLine;

import jp.co.future.uroborosql.store.SqlManager;

/**
 * SQL Name の補完を行うCompleter
 *
 * @author H.Sugimoto
 *
 */
public class SqlNameCompleter extends AbstractCompleter {
	private final SqlManager sqlManager;

	/**
	 * コンストラクタ
	 *
	 * @param sqlManager sqlManager
	 */
	public SqlNameCompleter(final SqlManager sqlManager) {
		this.sqlManager = sqlManager;
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
		String buffer = line.line();
		String[] parts = getLineParts(buffer);
		int len = parts.length;

		// コード補完する引数の番号を特定。
		int startArgNo = getStartArgNo(line);

		// 対象引数が-1、または開始引数にlenが満たない場合は該当なしなのでコード補完しない
		if (!accept(startArgNo, buffer, len)) {
			return;
		}

		boolean isBlank = buffer.endsWith(" ");
		SortedSet<String> sqlNames = new TreeSet<>(sqlManager.getSqlPathList());

		if (len == startArgNo && isBlank || len == startArgNo + 1 && !isBlank) {
			// コマンドが引数ありの場合
			String args = len == startArgNo + 1 ? parts[startArgNo] : "";
			if (StringUtils.isEmpty(args)) {
				candidates.addAll(sqlNames.stream().map(Candidate::new).collect(Collectors.toList()));
			} else {
				for (String match : sqlNames.tailSet(args)) {
					if (!match.startsWith(args)) {
						break;
					}
					candidates.add(new Candidate(match));
				}
			}
		}
	}
}