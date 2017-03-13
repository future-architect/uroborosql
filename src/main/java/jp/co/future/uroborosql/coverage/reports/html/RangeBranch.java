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

	RangeBranch(Range range) {
		this.range = range;
	}

	void add(BranchCoverageState state) {
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