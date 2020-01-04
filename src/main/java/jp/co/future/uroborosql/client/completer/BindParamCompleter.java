/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.completer;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.jline.reader.Candidate;
import org.jline.reader.LineReader;
import org.jline.reader.ParsedLine;

import jp.co.future.uroborosql.client.SqlParamUtils;
import jp.co.future.uroborosql.client.command.ReplCommand;
import jp.co.future.uroborosql.config.SqlConfig;

/**
 * バインドパラメータを補完するCompleter
 *
 * @author H.Sugimoto
 *
 */
public class BindParamCompleter extends AbstractCompleter {
	private final SqlConfig sqlConfig;

	/**
	 * Constructor
	 *
	 * @param commands ReplCommand List
	 * @param sqlConfig SqlConfig
	 */
	public BindParamCompleter(final List<ReplCommand> commands, final SqlConfig sqlConfig) {
		super(commands);
		this.sqlConfig = sqlConfig;
	}

	/**
	 * バッファから渡されたSQL名のSQLを取得し、バインドパラメータをパースして取得する。<br>
	 * 取得したバインドパラメータと入力を比較し、前方一致する場合は補完候補にする。<br>
	 * ただし、すでに指定済みのバインドパラメータは補完候補から除く<br>
	 *
	 * {@inheritDoc}
	 *
	 * @see org.jline.reader.Completer#complete(org.jline.reader.LineReader, org.jline.reader.ParsedLine, java.util.List)
	 */
	@Override
	public void complete(final LineReader reader, final ParsedLine line, final List<Candidate> candidates) {
		String buffer = line.line().substring(0, line.cursor());
		String[] parts = getLineParts(buffer);
		int pos = buffer.length();
		int len = parts.length;

		// コード補完する引数の番号を特定。
		int startArgNo = getStartArgNo(line);

		// 対象引数が-1、または開始引数にlenが満たない場合は該当なしなのでコード補完しない
		if (!accept(startArgNo, buffer, len)) {
			return;
		}

		boolean isBlank = buffer.endsWith(" ");
		// sqlNameが指定されている場合
		String sqlName = parts[startArgNo - 1];
		String sql = sqlConfig.getSqlManager().getSql(sqlName);
		Set<String> params = SqlParamUtils.getSqlParams(sql, sqlConfig);
		if (len > startArgNo) {
			// 最後のパラメータ以外ですでに指定されたバインドパラメータを候補から除去する
			int lastPos = isBlank ? len : len - 1;
			for (int i = startArgNo; i < lastPos; i++) {
				String part = parts[i];
				String[] keyValue = part.split("=", 2);
				params.remove(keyValue[0]);
			}
			if (isBlank) {
				// 候補の表示位置を計算
				candidates.addAll(
						params.stream().map(p -> new Candidate(p + "=", p, null, null, null, null, false))
								.collect(Collectors.toList()));
			} else {
				// 候補の表示位置を計算
				pos = pos - parts[len - 1].length();
				// 最後のパラメータについて候補を作成
				String[] keyValue = parts[len - 1].split("=", 2);
				if (keyValue.length == 2) {
					// すでに値の入力があるため補完は行わない
					pos = -1;
				} else {
					String key = keyValue[0];
					for (String match : params) {
						if (match.startsWith(key)) {
							candidates
									.add(new Candidate(match + "=", match, null, null, null, null, false));
						}
					}
				}
			}
		} else {
			candidates.addAll(
					params.stream().map(p -> new Candidate(p + "=", p, null, null, null, null, false))
							.collect(Collectors.toList()));
		}
	}
}