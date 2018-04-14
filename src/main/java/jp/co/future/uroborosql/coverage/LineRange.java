/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.coverage;

/**
 * 行Range情報
 *
 * @author ota
 */
public final class LineRange extends Range {
	private final int lineIndex;

	/**
	 * コンストラクタ
	 *
	 * @param start start
	 * @param end end
	 * @param lineIndex 行Index
	 */
	public LineRange(final int start, final int end, final int lineIndex) {
		super(start, end);
		this.lineIndex = lineIndex;
	}

	/**
	 * 行Index取得
	 *
	 * @return 行Index
	 */
	public int getLineIndex() {
		return lineIndex;
	}

}
