/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.coverage.reports.html;

import java.util.EnumSet;
import java.util.Set;

import jp.co.future.uroborosql.coverage.BranchCoverageState;
import jp.co.future.uroborosql.coverage.Range;

/**
 * Rangeブランチ情報
 */
class RangeBranch {
	private final Range range;
	private final Set<BranchCoverageState> status = EnumSet.noneOf(BranchCoverageState.class);

	RangeBranch(final Range range) {
		this.range = range;
	}

	void add(final BranchCoverageState state) {
		status.add(state);
	}

	int coveredSize() {
		return BranchCoverageState.getCoveredSize(status);
	}

	int branchSize() {
		return 2;
	}

	Range getRange() {
		return range;
	}
}