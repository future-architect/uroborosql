/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.regex.Pattern;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.node.BindVariableNode;
import jp.co.future.uroborosql.node.EmbeddedValueNode;
import jp.co.future.uroborosql.node.IfNode;
import jp.co.future.uroborosql.node.Node;
import jp.co.future.uroborosql.node.ParenBindVariableNode;
import jp.co.future.uroborosql.parser.ContextTransformer;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;
import jp.co.future.uroborosql.utils.StringUtils;
import ognl.ASTProperty;
import ognl.Ognl;
import ognl.OgnlException;

/**
 * Sqlのバインドパラメータを操作するユーティリティ
 *
 * @author H.Sugimoto
 *
 */
public final class SqlParamUtils {
	/** バインドパラメータ中の定数指定を判定するための正規表現 */
	private static final Pattern CONSTANT_PAT = Pattern.compile("^[A-Z][A-Z0-9_-]*$");

	/** 数字かどうかを判定するための正規表現 */
	private static final Pattern NUMBER_PAT = Pattern.compile("^[\\-\\+]?[1-9][0-9]*([Ll]|\\.\\d+[FfDd])?$");

	/**
	 * コンストラクタ
	 */
	private SqlParamUtils() {
		// do nothing
	}

	/**
	 * SQLバインドパラメータを設定する
	 *
	 * @param ctx SQLコンテキスト
	 * @param paramsArray パラメータ配列
	 */
	public static void setSqlParams(final SqlContext ctx, final String... paramsArray) {
		Set<String> bindParams = getSqlParams(ctx.getSql());

		for (String element : paramsArray) {
			String[] param = element.split("=");
			String key = param[0];
			if (bindParams.remove(key)) {
				// キーがバインドパラメータに存在するときは値を設定する
				if (param.length == 1) {
					// キーだけの指定は値をnullと扱う
					ctx.param(key, null);
				} else {
					String val = param[1];
					setParam(ctx, key, val);
				}
			}
		}

		// 指定がなかったキーについてはnullを設定する
		bindParams.forEach(s -> ctx.param(s, null));
	}

	/**
	 * 1つのパラメータの設定
	 *
	 * パラメータ値は以下の表記が可能
	 * <dl>
	 * 	<dh>[NULL]</dh>
	 *  <dd><code>null</code>を設定する</dd>
	 * 	<dh>[EMPTY]</dh>
	 *  <dd>""（空文字）を設定する</dd>
	 *  <dh>'値'</dh>
	 *  <dd>文字列として設定する. 空白を含めることもできる</dd>
	 * 	<dh>[値1,値2,...]</dh>
	 *  <dd>配列として設定する</dd>
	 * 	<dh>その他</dh>
	 *  <dd>文字列として設定する</dd>
	 * </dl>
	 *
	 * @param ctx SqlContext
	 * @param key パラメータキー
	 * @param val パラメータ値
	 */
	private static void setParam(final SqlContext ctx, final String key, final String val) {
		if (val.startsWith("[") && val.endsWith("]") && !(val.equals("[NULL]") || val.equals("[EMPTY]"))) {
			// [] で囲まれた値は配列に変換する。ex) [1, 2] => {"1", "2"}
			String[] parts = val.substring(1, val.length() - 1).split("\\s*,\\s*");
			Object[] vals = new Object[parts.length];
			for (int i = 0; i < parts.length; i++) {
				vals[i] = convertSingleValue(parts[i]);
			}
			ctx.param(key, Arrays.asList(vals));
		} else {
			ctx.param(key, convertSingleValue(val));
		}
	}

	/**
	 * パラメータで渡された単独の値を型変換する
	 *
	 * @param val 値の文字列
	 * @return 変換後オブジェクト
	 */
	private static Object convertSingleValue(final String val) {
		String value = val == null ? null : val.trim();
		if (StringUtils.isEmpty(value)) {
			return null;
		} else if ("[NULL]".equalsIgnoreCase(value)) {
			return null;
		} else if ("[EMPTY]".equalsIgnoreCase(value)) {
			return "";
		} else if (value.startsWith("'") && value.endsWith("'")) {
			// ''で囲まれた値は文字列として扱う。空白を含むこともできる。 ex) 'This is a pen'
			return value.substring(1, value.length() - 1);
		} else if (Boolean.TRUE.toString().equalsIgnoreCase(value)) {
			return Boolean.TRUE;
		} else if (Boolean.FALSE.toString().equalsIgnoreCase(value)) {
			return Boolean.FALSE;
		} else if (isNumber(value)) {
			return createNumber(value);
		} else {
			try {
				// 日時に変換できるか
				return LocalDateTime.parse(value, DateTimeFormatter.ISO_DATE_TIME);
			} catch (DateTimeParseException ex) {
				// do nothing
			}

			try {
				// 日付に変換できるか？
				return LocalDate.parse(value, DateTimeFormatter.ISO_DATE);
			} catch (DateTimeParseException ex) {
				// do nothing
			}

			try {
				// 時刻に変換できるか？
				return LocalTime.parse(value, DateTimeFormatter.ISO_TIME);
			} catch (DateTimeParseException ex) {
				// do nothing
			}
			return value;
		}
	}

	/**
	 * 判定対象文字列が数字かどうかを判定する.
	 *
	 * @param val 判定対象文字列
	 * @return 数字の場合は<code>true</code>
	 */
	private static boolean isNumber(final String val) {
		if (StringUtils.isEmpty(val)) {
			return false;
		} else {
			return NUMBER_PAT.matcher(val).matches();
		}
	}

	/**
	 * 指定された文字列から適切なNumber型のオブジェクトを生成する.<br>
	 * <ul>
	 * <li>1000 -> (Integer)1000</li>
	 * <li>+1000 -> (Integer)1000</li>
	 * <li>-1000 -> (Integer)-1000</li>
	 * <li>1000L -> (Long)1000L</li>
	 * <li>+1000L -> (Long)1000L</li>
	 * <li>-1000L -> (Long)-1000L</li>
	 * <li>1000.01F -> (Float)1000.01F</li>
	 * <li>+1000.01F -> (Float)1000.01F</li>
	 * <li>-1000.01F -> (Float)-1000.01F</li>
	 * <li>1000.01D -> (Float)1000.01D</li>
	 * <li>+1000.01D -> (Float)1000.01D</li>
	 * <li>-1000.01D -> (Float)-1000.01D</li>
	 * </ul>
	 * （※）各Number型で桁あふれした場合はBigDecimal型が返却される
	 *
	 * @param val 変換対象文字列
	 * @return Number型のオブジェクト
	 */
	private static Number createNumber(final String val) {
		// suffixがある場合はsuffixと数値部分を分離する
		String suffix = val.substring(val.length() - 1);
		String num = val;
		if ("0".compareTo(suffix) <= 0 && "9".compareTo(suffix) >= 0) {
			suffix = "";
		} else {
			num = val.substring(0, val.length() - 1);
		}

		BigDecimal decimal = new BigDecimal(num);
		try {
			if ("L".equalsIgnoreCase(suffix)) {
				return decimal.longValueExact();
			} else if ("F".equalsIgnoreCase(suffix)) {
				return decimal.floatValue();
			} else if ("D".equalsIgnoreCase(suffix)) {
				return decimal.doubleValue();
			} else {
				return decimal.intValueExact();
			}
		} catch (ArithmeticException ex) {
			// BigDecimalから指定の型に変換できない場合はBigDecimalを返す
			return decimal;
		}
	}

	/**
	 * SQLパラメータの解析
	 *
	 * @param sql 解析対象SQL
	 * @return SQLを解析して取得したパラメータキーのセット
	 */
	public static Set<String> getSqlParams(final String sql) {
		SqlParser parser = new SqlParserImpl(sql);
		ContextTransformer transformer = parser.parse();
		Node rootNode = transformer.getRoot();

		Set<String> params = new LinkedHashSet<>();
		traverseNode(rootNode, params);
		params.removeIf(s -> CONSTANT_PAT.matcher(s).matches());
		return params;
	}

	/**
	 * SQLの探索
	 *
	 * @param node SQLノード
	 * @param params パラメータが見つかった場合に格納するSetオブジェクト
	 */
	private static void traverseNode(final Node node, final Set<String> params) {
		if (node instanceof BindVariableNode) {
			params.add(((BindVariableNode) node).getExpression());
		} else if (node instanceof ParenBindVariableNode) {
			params.add(((ParenBindVariableNode) node).getExpression());
		} else if (node instanceof EmbeddedValueNode) {
			params.add(((EmbeddedValueNode) node).getExpression());
		} else if (node instanceof IfNode) {
			try {
				String expression = ((IfNode) node).getExpression();
				ognl.Node ognlNode = (ognl.Node) Ognl.parseExpression(expression);
				traverseExpression(ognlNode, params);
			} catch (OgnlException e) {
				e.printStackTrace();
			}
		}

		for (int i = 0; i < node.getChildSize(); i++) {
			Node childNode = node.getChild(i);
			traverseNode(childNode, params);
		}
	}

	/**
	 * 評価式の探索
	 *
	 * @param node SQLノード
	 * @param params パラメータが見つかった場合に格納するSetオブジェクト
	 */
	private static void traverseExpression(final ognl.Node node, final Set<String> params) {
		if (node == null) {
			return;
		}
		if (node instanceof ASTProperty) {
			ASTProperty prop = (ASTProperty) node;
			params.add(prop.toString());
		} else {
			int childCount = node.jjtGetNumChildren();
			for (int i = 0; i < childCount; i++) {
				ognl.Node child = node.jjtGetChild(i);
				traverseExpression(child, params);
			}
		}
	}
}
