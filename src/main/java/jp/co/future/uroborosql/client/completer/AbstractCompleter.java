/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.completer;

import java.util.ArrayList;
import java.util.List;

import org.jline.reader.Completer;
import org.jline.reader.ParsedLine;

import jp.co.future.uroborosql.client.command.ReplCommand;
import jp.co.future.uroborosql.utils.StringUtils;

public abstract class AbstractCompleter implements Completer {
	private final List<ReplCommand> commands;

	/**
	 * Constructor
	 * @param commands ReplCommand List
	 */
	protected AbstractCompleter(final List<ReplCommand> commands) {
		this.commands = commands;
	}

	/**
	 * このCompleterの開始位置を取得する
	 * @param line コマンドをパースしたオブジェクト
	 * @return このCompleterの開始位置。該当しない場合は<code>-1</code>を返す
	 */
	protected int getStartArgNo(final ParsedLine line) {
		// コード補完する引数の番号を特定。
		if (line.wordIndex() >= 1) {
			return commands.stream().filter(c -> c.is(line.words().get(0)))
					.mapToInt(c -> c.getStartArgNo(this.getClass())).findFirst().orElse(-1);
		} else {
			return -1;
		}
	}

	/**
	 * コード補完対象かどうかを判定する
	 * @param startArgNo 開始引数No
	 * @param buffer 入力文字列
	 * @param len partsのlength
	 * @return コード補完対象の場合<code>true</code>
	 */
	protected boolean accept(final int startArgNo, final String buffer, final int len) {
		// 対象引数が-1、または開始引数にlenが満たない場合は該当なしなのでコード補完しない
		return startArgNo != -1 && (buffer.endsWith(" ") && startArgNo <= len
				|| !buffer.endsWith(" ") && startArgNo + 1 <= len);
	}

	/**
	 * 入力行をパーツに分解する。空白で分解した後に条件によって結合を行う。<br>
	 *
	 * 【結合条件】<br>
	 * ・''で囲まれる値には空白を含めることができる。ex) 'String value includes whitespace.'<br>
	 * ・[]で囲まれる値は配列を表す。 ex) [10, 20, 30, 40], ['aaa' , 'bbb' , 'ccc']
	 *
	 * @param line 入力行
	 * @return パーツに分解した文字列配列
	 */
	protected String[] getLineParts(final String line) {
		var parts = line.split(" ");
		List<String> ans = new ArrayList<>();
		var idx = 0;
		var len = parts.length;
		while (idx < len) {
			if (parts[idx].contains("='") && !parts[idx].endsWith("'")) {
				var builder = new StringBuilder(parts[idx++]);
				while (idx < len) {
					builder.append(" ").append(parts[idx]);
					if (parts[idx++].endsWith("'")) {
						break;
					}
				}
				ans.add(builder.toString());
			} else if (parts[idx].contains("=[") && !parts[idx].endsWith("]")) {
				var builder = new StringBuilder(parts[idx++]);
				while (idx < len) {
					builder.append(" ").append(parts[idx]);
					if (parts[idx++].endsWith("]")) {
						break;
					}
				}
				ans.add(builder.toString());
			} else {
				var part = parts[idx++];
				if (StringUtils.isNotEmpty(part)) {
					ans.add(part);
				}
			}
		}
		return ans.toArray(new String[ans.size()]);
	}

}
