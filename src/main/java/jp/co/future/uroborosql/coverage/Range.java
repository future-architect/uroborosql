package jp.co.future.uroborosql.coverage;

/**
 * Range情報
 */
public class Range {
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
	public boolean intersection(final Range range) {
		return Math.max(this.start, range.start) <= Math.min(this.end, range.end);
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj == null || obj.getClass() != getClass()) {
			return false;
		} else {
			final Range range = (Range) obj;
			return start == range.start &&
					end == range.end;
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

}
