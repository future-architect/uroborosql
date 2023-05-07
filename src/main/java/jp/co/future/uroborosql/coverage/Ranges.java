/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.coverage;

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

/**
 * 集約Range情報
 *
 * @author ota
 */
public class Ranges extends AbstractSet<Range> {

	private final List<Range> ranges = new LinkedList<>();

	/**
	 * コンストラクタ
	 */
	public Ranges() {
	}

	/**
	 * コンストラクタ
	 *
	 * @param start 開始位置
	 * @param end 終了位置
	 */
	public Ranges(final int start, final int end) {
		add(new Range(start, end));
	}

	/**
	 * コンストラクタ
	 *
	 * @param ranges 初期データ
	 */
	public Ranges(final Collection<? extends Range> ranges) {
		addAll(ranges);
	}

	/**
	 * インスタンスコピー
	 *
	 * @return Copyインスタンス
	 */
	public Ranges copy() {
		return new Ranges(this);
	}

	@Override
	public ListIterator<Range> iterator() {
		Collections.sort(ranges);
		return ranges.listIterator();
	}

	@Override
	public boolean add(final Range range) {
		var target = range;
		for (var iterator = this.ranges.iterator(); iterator.hasNext();) {
			var r = iterator.next();
			if (r.equals(target) || r.include(target)) {
				return false;
			} else if (target.getEnd() + 1 == r.getStart()) {
				//吸収して元を削除
				target = new Range(target.getStart(), r.getEnd());
				iterator.remove();
			} else if (r.getEnd() + 1 == target.getStart()) {
				//吸収して元を削除
				target = new Range(r.getStart(), target.getEnd());
				iterator.remove();
			} else if (r.hasIntersection(target)) {
				//吸収して元を削除 本機能では通常通らないはず
				target = new Range(Math.min(r.getStart(), target.getStart()), Math.max(r.getEnd(), target.getEnd()));
				iterator.remove();
			}
		}
		ranges.add(target);
		return true;
	}

	@Override
	public String toString() {
		Collections.sort(ranges);
		return ranges.toString();
	}

	/**
	 * MINUS
	 *
	 * @param ranges Rangeコレクション
	 */
	public void minus(final Collection<? extends Range> ranges) {
		ranges.forEach(this::minus);
	}

	/**
	 * MINUS
	 *
	 * @param range Range
	 */
	public void minus(final Range range) {
		var newList = new ArrayList<Range>();
		var target = range;
		for (var iterator = this.ranges.iterator(); iterator.hasNext();) {
			var r = iterator.next();
			if (r.equals(target) || target.include(r)) {
				iterator.remove();
			} else if (r.include(target)) {
				if (r.getStart() < target.getStart()) {
					newList.add(new Range(r.getStart(), target.getStart() - 1));
				}
				if (r.getEnd() > target.getEnd()) {
					newList.add(new Range(target.getEnd() + 1, r.getEnd()));
				}
				iterator.remove();
			} else if (r.hasIntersection(target)) {
				if (r.include(target.getStart())) {
					newList.add(new Range(r.getStart(), target.getStart() - 1));
				} else {
					newList.add(new Range(target.getEnd() + 1, r.getEnd()));
				}
				iterator.remove();
			}
		}
		ranges.addAll(newList);
	}

	/**
	 * 積集合部のみにする
	 *
	 * @param ranges Rangeコレクション
	 */
	public void intersect(final Collection<? extends Range> ranges) {
		var targetRanges = ranges instanceof Ranges ? (Ranges) ranges : new Ranges(ranges);

		var newList = new ArrayList<Range>();
		for (var iterator = this.ranges.iterator(); iterator.hasNext();) {
			var r = iterator.next();
			var hasIntersections = getHasIntersections(targetRanges, r);
			if (hasIntersections.isEmpty()) {
				iterator.remove();
			} else {
				if (hasIntersections.size() == 1) {
					var target = hasIntersections.get(0);
					if (r.equals(target)) {
						continue;
					}
				}
				iterator.remove();
				for (var rangeHasIntersection : hasIntersections) {
					newList.add(rangeHasIntersection.intersection(r));
				}
			}
		}
		addAll(newList);
	}

	@Override
	public int size() {
		return ranges.size();
	}

	private List<Range> getHasIntersections(final Collection<Range> targetRanges, final Range r) {
		var ret = new ArrayList<Range>();
		for (var range : targetRanges) {
			if (range.hasIntersection(r)) {
				ret.add(range);
			} else if (r.getEnd() < range.getStart()) {
				return ret;
			}
		}
		return ret;
	}
}
