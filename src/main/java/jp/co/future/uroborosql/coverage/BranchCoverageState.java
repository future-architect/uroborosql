/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.coverage;

import java.util.Set;

/**
 * カバレッジの状態を表現する列挙体
 *
 * @author ota
 */
public enum BranchCoverageState {
	/** 条件true */
	TRUE(1),
	/** 条件false */
	FALSE(0),
	/** 未通過 */
	NOT_PASSED(9);

	private final int val;

	BranchCoverageState(final int val) {
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
	public static int getCoveredSize(final Set<BranchCoverageState> status) {
		var size = 0;
		if (status.contains(TRUE)) {
			size++;
		}
		if (status.contains(FALSE)) {
			size++;
		}
		return size;
	}
}