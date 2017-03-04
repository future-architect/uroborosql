package jp.co.future.uroborosql.filter;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;
import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * 実行結果をダンプ出力するSqlFilter
 *
 * @author H.Sugimoto
 *
 */
public class DumpResultSqlFilter extends AbstractSqlFilter {
	private static final String FRAME_STR = "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------";
	private static final String SPACE_STR = "                                                                                                                                                                                                        ";

	@Override
	public Parameter doParameter(final Parameter parameter) {
		return parameter;
	}

	@Override
	public Object doOutParameter(final String key, final Object val) {
		return val;
	}

	@Override
	public PreparedStatement doPreparedStatement(final SqlContext sqlContext,
			final PreparedStatement preparedStatement)
			throws SQLException {
		return preparedStatement;
	}

	@Override
	public CallableStatement doCallableStatement(final SqlContext sqlContext,
			final CallableStatement callableStatement)
			throws SQLException {
		return callableStatement;
	}

	@Override
	public ResultSet doQuery(final SqlContext sqlContext, final PreparedStatement preparedStatement,
			final ResultSet resultSet) {
		displayResult(resultSet);
		try {
			resultSet.beforeFirst();
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return resultSet;
	}

	/**
	 * 検索結果を表示
	 *
	 * @param rs 検索結果のResultSet
	 */
	private void displayResult(final ResultSet rs) {
		try {
			List<String> keys = new ArrayList<>();
			Map<String, Integer> maxLengthList = new HashMap<>();
			ResultSetMetaData rsmd = rs.getMetaData();
			int columnCount = rsmd.getColumnCount();
			for (int i = 1; i <= columnCount; i++) {
				String columnLabel = rsmd.getColumnLabel(i);
				keys.add(columnLabel);
				maxLengthList.put(columnLabel, getByteLength(columnLabel));
			}

			List<Map<String, Object>> rows = new ArrayList<>();

			while (rs.next()) {
				Map<String, Object> data = new HashMap<>();

				for (String key : keys) {
					Object val = rs.getObject(key);
					data.put(key, val);

					int currentLength = getByteLength(val);
					maxLengthList.compute(key, (k, v) -> v < currentLength ? currentLength : v);
				}
				rows.add(data);
			}

			StringBuilder builder = new StringBuilder();
			// ヘッダ部出力
			builder.append("+");
			for (String key : keys) {
				builder.append(FRAME_STR.substring(0, maxLengthList.get(key))).append("+");
			}
			builder.append(System.lineSeparator()).append("|");
			for (String key : keys) {
				builder.append(fillHeader(key, maxLengthList.get(key))).append("|");
			}
			builder.append(System.lineSeparator()).append("+");
			for (String key : keys) {
				builder.append(FRAME_STR.substring(0, maxLengthList.get(key))).append("+");
			}

			// データ部出力

			if (rows.isEmpty()) {
				builder.append(System.lineSeparator()).append("|");
				int len = 1;
				for (String key : keys) {
					len = len + maxLengthList.get(key) + 1;
				}

				if (len >= 13) {
					builder.append("empty data.").append(SPACE_STR.substring(0, len - 13)).append("|");
				} else {
					builder.append("-").append(SPACE_STR.substring(0, len - 2)).append("|");
				}
			} else {
				for (Map<String, Object> row : rows) {
					builder.append(System.lineSeparator()).append("|");
					for (String key : keys) {
						builder.append(fillData(row.get(key), maxLengthList.get(key))).append("|");
					}
				}

			}
			builder.append(System.lineSeparator()).append("+");
			for (String key : keys) {
				builder.append(FRAME_STR.substring(0, maxLengthList.get(key))).append("+");
			}

			System.out.println(builder.toString());

			// カーソルを先頭の前に戻す
			rs.beforeFirst();
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	private String fillHeader(final String str, final int length) {
		int strLen = getByteLength(str);
		int spaceSize = (length - strLen) / 2;

		String spaceStr = SPACE_STR.substring(0, spaceSize);
		String ans = spaceStr + str + spaceStr;
		int fillLen = getByteLength(ans);
		if (length > fillLen) {
			ans = ans + SPACE_STR.substring(0, (length - fillLen));
		}
		return ans;
	}

	private String fillData(final Object val, final int length) throws CharacterCodingException,
			UnsupportedEncodingException {
		int valLen = getByteLength(val);
		int spaceSize = length - valLen;

		if (val instanceof Number) {
			return SPACE_STR.substring(0, spaceSize) + getSubstringByte(val, length);
		} else {
			return getSubstringByte(val, length) + SPACE_STR.substring(0, spaceSize);
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
		String str = val.toString();
		try {
			int len = str.getBytes("Shift-JIS").length;
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

		String str = obj == null ? "null" : obj.toString();
		if (capacity < 1) {
			return str;
		}

		CharsetEncoder ce = Charset.forName("Shift-JIS").newEncoder()
				.onMalformedInput(CodingErrorAction.REPLACE).onUnmappableCharacter(CodingErrorAction.REPLACE).reset();
		if (capacity >= ce.maxBytesPerChar() * str.length()) {
			return str;
		}
		CharBuffer cb = CharBuffer.wrap(new char[Math.min(str.length(), capacity)]);
		str.getChars(0, Math.min(str.length(), cb.length()), cb.array(), 0);

		if (capacity >= ce.maxBytesPerChar() * cb.limit()) {
			return cb.toString();
		}
		ByteBuffer out = ByteBuffer.allocate(capacity);
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

	@Override
	public int doUpdate(final SqlContext sqlContext, final PreparedStatement preparedStatement,
			final int result) {
		return result;
	}

	@Override
	public int[] doBatch(final SqlContext sqlContext, final PreparedStatement preparedStatement,
			final int[] result) {
		return result;
	}

}
