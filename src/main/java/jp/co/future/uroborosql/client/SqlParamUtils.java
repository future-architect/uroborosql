/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client;

import java.sql.Time;
import java.sql.Timestamp;
import java.text.ParseException;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.commons.lang3.time.DateUtils;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.node.BindVariableNode;
import jp.co.future.uroborosql.node.EmbeddedValueNode;
import jp.co.future.uroborosql.node.IfNode;
import jp.co.future.uroborosql.node.Node;
import jp.co.future.uroborosql.node.ParenBindVariableNode;
import jp.co.future.uroborosql.parser.ContextTransformer;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;
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
			ctx.paramList(key, vals);
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
		String value = StringUtils.trim(val);
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
		} else if (NumberUtils.isCreatable(value)) {
			return NumberUtils.createNumber(value);
		} else {
			try {
				// 日時に変換できるか
				return new Timestamp(DateUtils.parseDateStrictly(value, "yyyy-MM-dd'T'HH:mm:ss").getTime());
			} catch (ParseException ex) {
				// do nothing
			}

			try {
				// 日付に変換できるか？
				return new java.sql.Date(DateUtils.parseDateStrictly(value, "yyyy-MM-dd").getTime());
			} catch (ParseException ex) {
				// do nothing
			}

			try {
				// 時刻に変換できるか？
				return new Time(DateUtils.parseDateStrictly(value, "HH:mm:ss").getTime());
			} catch (ParseException ex) {
				// do nothing
			}
			return value;
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
