/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.completer;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.jline.reader.Completer;
import org.jline.reader.ParsedLine;

import jp.co.future.uroborosql.client.ReplCommand;

public abstract class AbstractCompleter implements Completer {

	protected int getStartArgNo(final ParsedLine line) {
		// コード補完する引数の番号を特定。
		return line.wordIndex() >= 1 ? ReplCommand.toCommand(line.words().get(0)).getStartArgNo(this.getClass()) : -1;
	}

	/**
	 * コード補完対象かどうかを判定する
	 *
	 * @param startArgNo 開始引数No
	 * @param buffer 入力文字列
	 * @param len 入力パーツの個数
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
		String[] parts = line.split(" ");
		List<String> ans = new ArrayList<>();
		int idx = 0;
		int len = parts.length;
		while (idx < len) {
			if (parts[idx].contains("='") && !parts[idx].endsWith("'")) {
				StringBuilder builder = new StringBuilder(parts[idx++]);
				while (idx < len) {
					builder.append(" ").append(parts[idx]);
					if (parts[idx++].endsWith("'")) {
						break;
					}
				}
				ans.add(builder.toString());
			} else if (parts[idx].contains("=[") && !parts[idx].endsWith("]")) {
				StringBuilder builder = new StringBuilder(parts[idx++]);
				while (idx < len) {
					builder.append(" ").append(parts[idx]);
					if (parts[idx++].endsWith("]")) {
						break;
					}
				}
				ans.add(builder.toString());
			} else {
				String part = parts[idx++];
				if (StringUtils.isNotEmpty(part)) {
					ans.add(part);
				}
			}
		}
		return ans.toArray(new String[ans.size()]);
	}

}
