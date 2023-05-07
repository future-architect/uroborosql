/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.filter;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * 実行結果をダンプ出力するSqlFilter.<br>
 *
 * このSqlFilterを使用する際は、PreparedStatementを生成する際、ResultSetTypeに
 * <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code> または<code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
 * を指定してください。
 *
 * @author H.Sugimoto
 *
 */
public class DumpResultSqlFilter extends AbstractSqlFilter {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.filter");

	/** 文字数計算用のエンコーディング */
	private static final String ENCODING_SHIFT_JIS = "Shift-JIS";

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doQuery(jp.co.future.uroborosql.context.ExecutionContext, java.sql.PreparedStatement, java.sql.ResultSet)
	 */
	@Override
	public ResultSet doQuery(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
			final ResultSet resultSet) {
		try {
			if (resultSet.getType() == ResultSet.TYPE_FORWARD_ONLY) {
				LOG.warn(
						"ResultSet type is TYPE_FORWARD_ONLY. DumpResultSqlFilter use ResultSet#beforeFirst(). Please Set TYPE_SCROLL_INSENSITIVE or TYPE_SCROLL_SENSITIVE.");
			}
			if (LOG.isInfoEnabled()) {
				var builder = displayResult(resultSet);
				LOG.debug("{}", builder);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return resultSet;
	}

	/**
	 * 検索結果を表示
	 *
	 * @param rs 検索結果のResultSet
	 * @return 表示文字列
	 */
	public StringBuilder displayResult(final ResultSet rs) {
		try {
			var keys = new ArrayList<String>();
			var maxLengthList = new HashMap<String, Integer>();
			var rsmd = rs.getMetaData();
			var columnCount = rsmd.getColumnCount();
			for (var i = 1; i <= columnCount; i++) {
				var columnLabel = rsmd.getColumnLabel(i);
				keys.add(columnLabel);
				maxLengthList.put(columnLabel, getByteLength(columnLabel));
			}

			var rows = new ArrayList<Map<String, Object>>();

			while (rs.next()) {
				var data = new HashMap<String, Object>();

				for (var key : keys) {
					var val = rs.getObject(key);
					data.put(key, val);

					var currentLength = getByteLength(val);
					maxLengthList.compute(key, (k, v) -> v < currentLength ? currentLength : v);
				}
				rows.add(data);
			}

			var builder = new StringBuilder(System.lineSeparator());
			// ヘッダ部出力
			builder.append("+");
			for (var key : keys) {
				builder.append(StringUtils.repeat('-', maxLengthList.get(key))).append("+");
			}
			builder.append(System.lineSeparator()).append("|");
			for (var key : keys) {
				builder.append(fillHeader(key, maxLengthList.get(key))).append("|");
			}
			builder.append(System.lineSeparator()).append("+");
			for (var key : keys) {
				builder.append(StringUtils.repeat('-', maxLengthList.get(key))).append("+");
			}

			// データ部出力

			if (rows.isEmpty()) {
				builder.append(System.lineSeparator()).append("|");
				var len = 1;
				for (var key : keys) {
					len = len + maxLengthList.get(key) + 1;
				}

				if (len >= 13) {
					builder.append("empty data.").append(StringUtils.repeat(' ', len - 13)).append("|");
				} else {
					builder.append("-").append(StringUtils.repeat(' ', len - 3)).append("|");
				}
			} else {
				for (var row : rows) {
					builder.append(System.lineSeparator()).append("|");
					for (var key : keys) {
						builder.append(fillData(row.get(key), maxLengthList.get(key))).append("|");
					}
				}

			}
			builder.append(System.lineSeparator()).append("+");
			for (var key : keys) {
				builder.append(StringUtils.repeat('-', maxLengthList.get(key))).append("+");
			}

			// カーソルを先頭の前に戻す
			if (rs.getType() != ResultSet.TYPE_FORWARD_ONLY) {
				rs.beforeFirst();
			}

			return builder;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	private String fillHeader(final String str, final int length) {
		var strLen = getByteLength(str);
		var spaceSize = (length - strLen) / 2;

		var spaceStr = StringUtils.repeat(' ', spaceSize);
		var ans = spaceStr + str + spaceStr;
		var fillLen = getByteLength(ans);
		if (length > fillLen) {
			ans = ans + StringUtils.repeat(' ', length - fillLen);
		}
		return ans;
	}

	private String fillData(final Object val, final int length) throws CharacterCodingException,
			UnsupportedEncodingException {
		var valLen = getByteLength(val);
		var spaceSize = length - valLen;

		if (val instanceof Number) {
			return StringUtils.repeat(' ', spaceSize) + getSubstringByte(val, length);
		} else {
			return getSubstringByte(val, length) + StringUtils.repeat(' ', spaceSize);
		}

	}

	/**
	 * オブジェクトの文字列表現のバイト数（Shift-JIS換算）を取得する
	 *
	 * @param val 計算対象オブジェクト
	 * @return バイト数。200バイトを超える場合は200を返す
	 */
	private int getByteLength(final Object val) {
		if (val == null) {
			return 4;
		}
		var str = val.toString();
		try {
			var len = str.getBytes(ENCODING_SHIFT_JIS).length;
			return len <= 200 ? len : 200;
		} catch (UnsupportedEncodingException ex) {
			return 1;
		}
	}

	/**
	 * 指定したバイト数で文字列をカットする
	 *
	 * @param obj 対象オブジェクト
	 * @param capacity カットするバイト数
	 * @return String
	 * @throws CharacterCodingException
	 * @throws UnsupportedEncodingException
	 */
	private String getSubstringByte(final Object obj, final int capacity) throws CharacterCodingException,
			UnsupportedEncodingException {
		var str = obj == null ? "null" : obj.toString();
		if (capacity < 1) {
			return str;
		}

		var ce = Charset.forName(ENCODING_SHIFT_JIS).newEncoder()
				.onMalformedInput(CodingErrorAction.REPLACE).onUnmappableCharacter(CodingErrorAction.REPLACE).reset();
		if (capacity >= ce.maxBytesPerChar() * str.length()) {
			return str;
		}
		var cb = CharBuffer.wrap(new char[Math.min(str.length(), capacity)]);
		str.getChars(0, Math.min(str.length(), cb.length()), cb.array(), 0);

		if (capacity >= ce.maxBytesPerChar() * cb.limit()) {
			return cb.toString();
		}
		var out = ByteBuffer.allocate(capacity);
		ce.reset();
		CoderResult cr = null;
		if (cb.hasRemaining()) {
			cr = ce.encode(cb, out, true);
		} else {
			cr = CoderResult.UNDERFLOW;
		}
		if (cr.isUnderflow()) {
			cr = ce.flush(out);
		}
		return cb.flip().toString();
	}
}
