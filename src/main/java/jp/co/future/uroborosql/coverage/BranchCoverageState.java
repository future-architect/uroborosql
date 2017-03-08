package jp.co.future.uroborosql.coverage;

import java.util.Set;

/**
 * カバレッジの状態を表現する列挙体
 */
public enum BranchCoverageState {
	/** 条件true */
	TRUE(1),
	/** 条件false */
	FALSE(0),
	/** 未通過 */
	NOT_PASSED(9);

	private final int val;

	private BranchCoverageState(final int val) {
		this.val = val;
	}

	@Override
	public String toString() {
		return String.valueOf(this.val);
	}

	/**
	 * カバーサイズの取得
	 * 
	 * @param status Set
	 * @return カバーサイズ
	 */
	public static int getCoveredSize(Set<BranchCoverageState> status) {
		int size = 0;
		if (status.contains(TRUE)) {
			size++;
		}
		if (status.contains(FALSE)) {
			size++;
		}
		return size;
	}
}