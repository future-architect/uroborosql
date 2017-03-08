package jp.co.future.uroborosql.coverage;

import java.util.ArrayList;
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
	 * 各行のchar index範囲を取得
	 *
	 * @param sql
	 * @return 各行のRange
	 */
	static List<LineRange> buildLineRanges(String sql) {
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
}
