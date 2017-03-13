package jp.co.future.uroborosql.coverage;

import java.util.Comparator;

/**
 * Range情報
 *
 * @author ota
 */
public class Range implements Comparable<Range> {
	private static final Comparator<Range> COMPARATOR = Comparator.comparingInt(Range::getStart)
			.thenComparingInt(Range::getEnd);

	private final int start;
	private final int end;

	/**
	 * コンストラクタ
	 *
	 * @param start start
	 * @param end end
	 */
	public Range(final int start, final int end) {
		if (start < end) {
			this.start = start;
			this.end = end;
		} else {
			this.start = end;
			this.end = start;
		}
	}

	/**
	 * Range開始位置の取得
	 *
	 * @return Range開始位置
	 */
	public int getStart() {
		return start;
	}

	/**
	 * Range終了位置の取得
	 *
	 * @return Range終了位置
	 */
	public int getEnd() {
		return end;
	}

	/**
	 * 位置がRangeに含まれるか判定
	 *
	 * @param index 判定位置
	 * @return true 含まれる
	 */
	public boolean contains(final int index) {
		return start <= index && index <= end;
	}

	/**
	 * 交差判定
	 *
	 * @param range 判定Range
	 * @return 交差
	 */
	public boolean hasIntersection(final Range range) {
		return Math.max(this.start, range.start) <= Math.min(this.end, range.end);
	}

	/**
	 * 共通部分を取り出す
	 *
	 * @param range 判定Range
	 * @return {@link Range} 共通部が無い場合はnull
	 */
	public Range intersection(final Range range) {
		int c0 = Math.max(this.start, range.start);
		int c1 = Math.min(this.end, range.end);
		return c0 <= c1 ? new Range(c0, c1) : null;
	}

	/**
	 * 内包判定
	 *
	 * @param range 判定Range
	 * @return 内包
	 */
	public boolean include(Range range) {
		return start <= range.start && range.end <= end;
	}

	/**
	 * 内包判定
	 *
	 * @param i 判定ポジション
	 * @return 内包
	 */
	public boolean include(int i) {
		return start <= i && i <= end;
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj == null || obj.getClass() != getClass()) {
			return false;
		} else {
			final Range range = (Range) obj;
			return start == range.start && end == range.end;
		}
	}

	@Override
	public int hashCode() {
		int result = 0;
		result = 17;
		result = 37 * result + getClass().hashCode();
		result = 37 * result + Integer.hashCode(start);
		result = 37 * result + Integer.hashCode(end);
		return result;
	}

	@Override
	public String toString() {
		return "[" + start + ".." + end + "]";
	}

	@Override
	public int compareTo(Range o) {
		return COMPARATOR.compare(this, o);
	}

}
