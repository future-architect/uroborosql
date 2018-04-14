/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.coverage;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

/**
 * カバレッジ情報ハンドラ
 *
 * @author ota
 */
public interface CoverageHandler {
	/**
	 * カバレッジ情報ハンドラ
	 *
	 * @param coverageData カバレッジ情報
	 */
	void accept(CoverageData coverageData);

	/**
	 * {@link jp.co.future.uroborosql.SqlAgent SqlAgent}のcloseのタイミングでcallされるメソッド
	 */
	default void onSqlAgentClose() {
	}

	/**
	 * 各行のindex範囲を取得
	 *
	 * @param sql SQL
	 * @return 各行のRange
	 */
	static List<LineRange> getLineRanges(String sql) {
		List<LineRange> ret = new ArrayList<>();
		int start = 0;
		int searchStart = 0;
		try (Scanner scanner = new Scanner(sql)) {
			while (scanner.hasNextLine()) {
				String line = scanner.nextLine();
				while (!sql.startsWith(line, searchStart)) {
					searchStart++;
				}
				LineRange range = new LineRange(start, searchStart + line.length() - 1, ret.size());
				ret.add(range);
				start = searchStart + line.length();
				searchStart = start;
			}
		}
		return ret;
	}

	/**
	 * 各行のindex範囲を取得しカバレッジに不要な行を除外する
	 *
	 * @param sql SQL
	 * @return 各行のRange
	 */
	static List<LineRange> parseLineRanges(String sql) {
		List<LineRange> ret = getLineRanges(sql);
		for (Iterator<LineRange> iterator = ret.iterator(); iterator.hasNext();) {
			LineRange lineRange = iterator.next();
			String line = sql.substring(lineRange.getStart(), lineRange.getEnd() + 1);
			String trimLine = line.trim();
			if (trimLine.isEmpty()) {
				iterator.remove();
			} else if (trimLine.equals("/*END*/") || trimLine.equals("/*ELSE*/")) {
				iterator.remove();
			} else {
				if (trimLine.startsWith("--")) {
					String comments = trimLine.substring(2);
					if (comments.trim().equals("ELSE")) {
						iterator.remove();
					}
				}
			}

		}
		return ret;
	}

	/**
	 * パーセント計算
	 *
	 * @param covered カバー数
	 * @param valid 対象数
	 * @return カバレッジ パーセント
	 */
	static int percent(int covered, int valid) {
		if (valid == 0) {
			return 100;
		}
		return covered * 100 / valid;
	}

	/**
	 * パーセント計算
	 *
	 * @param covered カバー数
	 * @param valid 対象数
	 * @return カバレッジ パーセント（文字列）
	 */
	static String percentStr(int covered, int valid) {
		return String.valueOf(percent(covered, valid));
	}
}
