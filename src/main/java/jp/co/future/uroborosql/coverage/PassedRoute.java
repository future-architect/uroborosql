/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.coverage;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

/**
 * 変換したSQLで評価された分岐情報
 *
 * @author ota
 */
public class PassedRoute {
	private final Map<Range, BranchCoverageState> passed = new HashMap<>();
	private final List<Range> hits = new ArrayList<>();
	private Ranges ranges;

	/**
	 * 分岐情報追加
	 *
	 * @param start 開始位置（文字index）
	 * @param end 終了位置（文字index）
	 * @param state カバレッジ状態
	 */
	public void appendBranchState(final int start, final int end, final BranchCoverageState state) {
		this.passed.put(new Range(start, end), state);
	}

	/**
	 * 通過範囲の追加
	 *
	 * @param start 開始位置（文字index）
	 * @param end 終了位置（文字index）
	 */
	public void appendHitRange(final int start, final int end) {
		hits.add(new Range(start, end));
		ranges = null;//キャッシュをクリア
	}

	/**
	 * 分岐情報取得
	 *
	 * @return 分岐情報
	 */
	public Map<Integer, BranchCoverageState> getBranchStatus() {
		Map<Integer, BranchCoverageState> map = new HashMap<>();
		passed.forEach((r, s) -> map.put(r.getStart(), s));
		return Collections.unmodifiableMap(map);
	}

	/**
	 * 分岐情報取得
	 *
	 * @return 分岐情報
	 */
	public Map<Range, BranchCoverageState> getRangeBranchStatus() {
		return Collections.unmodifiableMap(passed);
	}

	/**
	 * 通過情報取得
	 *
	 * @return 通過情報
	 */
	public Collection<Range> getHitRanges() {
		return Collections.unmodifiableCollection(getRanges());
	}

	/**
	 * 指定した位置が通過した箇所かを判定する
	 *
	 * @param index 判定位置
	 * @return 通過
	 */
	public boolean isHit(final int index) {
		for (var range : getRanges()) {
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
	public boolean isHit(final Range target) {
		for (var range : getRanges()) {
			if (range.hasIntersection(target)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public String toString() {
		return getBranchStatus().entrySet().stream().sorted(Comparator.comparingInt(Entry::getKey))
				.map(e -> e.getKey() + ":" + e.getValue()).collect(Collectors.joining(",", "{", "}"));
	}

	private Ranges getRanges() {
		if (ranges == null) {
			ranges = new Ranges(hits);
		}
		return ranges;
	}
}