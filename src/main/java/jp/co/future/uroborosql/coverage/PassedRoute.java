package jp.co.future.uroborosql.coverage;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 変換したSQLで評価された分岐情報
 *
 * @author ota
 */
public class PassedRoute {
	private final Map<Integer, CoverageState> passed = new HashMap<>();
	private final List<Range> hits = new ArrayList<>();

	/**
	 * 分岐情報追加
	 *
	 * @param position 分岐出現ポジション（文字index）
	 * @param state カバレッジ状態
	 */
	public void append(int position, CoverageState state) {
		this.passed.put(position, state);
	}

	/**
	 * 通過範囲の追加
	 *
	 * @param start 開始位置（文字index）
	 * @param end 終了位置（文字index）
	 */
	public void appendHitRange(int start, int end) {
		hits.add(new Range(start, end));
	}

	/**
	 * 分岐情報取得
	 *
	 * @return 分岐情報
	 */
	public Map<Integer, CoverageState> getPassed() {
		return passed;
	}

	/**
	 * 指定した位置が通過した箇所かを判定する
	 *
	 * @param index 判定位置
	 * @return 通過
	 */
	public boolean isHit(int index) {
		for (Range range : hits) {
			if (range.contains(index)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 指定した範囲が通過した箇所かを判定する
	 *
	 * @param target 判定範囲
	 * @return 通過
	 */
	public boolean isHit(Range target) {
		for (Range range : hits) {
			if (range.intersection(target)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public String toString() {
		return this.passed.entrySet().stream()
				.sorted(Comparator.comparingInt(e -> e.getKey()))
				.map(e -> e.getKey() + ":" + e.getValue())
				.collect(Collectors.joining(",", "{", "}"));
	}
}