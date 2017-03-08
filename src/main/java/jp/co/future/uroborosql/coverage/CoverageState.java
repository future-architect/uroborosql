package jp.co.future.uroborosql.coverage;

/**
 * カバレッジの状態を表現する列挙体
 */
public enum CoverageState {
	/** 通過 */
	PASSED(1),
	/** 未通過 */
	FAILED(0);

	private final int val;

	private CoverageState(final int val) {
		this.val = val;
	}

	@Override
	public String toString() {
		return String.valueOf(this.val);
	}
}