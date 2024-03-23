/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.utils;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;

import org.springframework.util.StringUtils;

public final class ObjectUtils {
	private ObjectUtils() {
	}

	/**
	 * 指定したオブジェクトが空かどうかを判定する.<br>
	 * 指定したオブジェクトがOptional型の場合、その中身を評価する.<br>
	 * 以下の場合に空と判定する.
	 * <ul>
	 * <li>null</li>
	 * <li>空文字</li>
	 * <li>空配列</li>
	 * <li>空Collection</li>
	 * <li>空Map</li>
	 * </ul>
	 *
	 * <pre>
	 * isEmpty(null)                      = true
	 * isEmpty("")                        = true
	 * isEmpty(" ")                       = false
	 * isEmpty("bob")                     = false
	 * isEmpty("  bob  ")                 = false
	 * isEmpty(new String[0])             = true
	 * isEmpty(new String[]{ "a" })       = false
	 * isEmpty(List.of())                 = true
	 * isEmpty(List.of("aa"))             = false
	 * isEmpty(Map.of())                  = true
	 * isEmpty(Map.of("aa", "bb"))        = false
	 * isEmpty(Optional.empty())          = true
	 * isEmpty(Optional.of(List.of()))    = true
	 * isEmpty(Optional.of(List.of("a"))) = false
	 * </pre>
	 *
	 * @see Optional#isEmpty()
	 * @see Collection#isEmpty()
	 * @see Map#isEmpty()
	 *
	 * @param obj 対象オブジェクト
	 * @return null / 空文字 / 空配列 / 空Collection / 空Map の場合<code>true</code>
	 */
	public static boolean isEmpty(final Object obj) {
		var val = obj instanceof Optional ? ((Optional<?>) obj).orElse(null) : obj;
		if (val == null) {
			return true;
		} else if (val instanceof CharSequence) {
			// 文字列
			return ((CharSequence) val).length() == 0;
		} else if (val instanceof Collection) {
			// Collection
			return ((Collection<?>) val).isEmpty();
		} else if (val instanceof Map) {
			// Map
			return ((Map<?, ?>) val).isEmpty();
		} else if (val.getClass().isArray()) {
			// 配列
			return Array.getLength(val) == 0;
		} else {
			return false;
		}
	}

	/**
	 * 指定したオブジェクトが空でないことを判定する.<br>
	 * 指定したオブジェクトがOptional型の場合、その中身を評価する.<br>
	 * 以下の場合に空と判定する.
	 * <ul>
	 * <li>null</li>
	 * <li>空文字</li>
	 * <li>空配列</li>
	 * <li>空Collection</li>
	 * <li>空Map</li>
	 * </ul>
	 *
	 * <pre>
	 * isEmpty(null)                      = false
	 * isEmpty("")                        = false
	 * isEmpty(" ")                       = true
	 * isEmpty("bob")                     = true
	 * isEmpty("  bob  ")                 = true
	 * isEmpty(new String[0])             = false
	 * isEmpty(new String[]{ "a" })       = true
	 * isEmpty(List.of())                 = false
	 * isEmpty(List.of("aa"))             = true
	 * isEmpty(Map.of())                  = false
	 * isEmpty(Map.of("aa", "bb"))        = true
	 * isEmpty(Optional.empty())          = false
	 * isEmpty(Optional.of(List.of()))    = false
	 * isEmpty(Optional.of(List.of("a"))) = true
	 * </pre>
	 *
	 * @see Optional#isEmpty()
	 * @see Collection#isEmpty()
	 * @see Map#isEmpty()
	 *
	 * @param obj 対象オブジェクト
	 * @return null / 空文字 / 空配列 / 空Collection / 空Map 以外の場合<code>true</code>
	 */
	public static boolean isNotEmpty(final Object obj) {
		return !isEmpty(obj);
	}

	/**
	 * 文字列の先頭文字を大文字にする
	 *
	 * <pre>
	 * capitalize(null)  = null
	 * capitalize("")    = ""
	 * capitalize("cat") = "Cat"
	 * capitalize("cAt") = "CAt"
	 * </pre>
	 *
	 * @see #uncapitalize(String)
	 *
	 * @param str 文字列
	 * @return 先頭を大文字にした文字列
	 */
	public static String capitalize(final String str) {
		if (str == null) {
			return null;
		} else if ("".equals(str)) {
			return "";
		} else {
			if (str.length() == 1) {
				return str.toUpperCase();
			} else {
				return str.substring(0, 1).toUpperCase() + str.substring(1);
			}
		}
	}

	/**
	 * 文字列の先頭を小文字にする
	 *
	 * <pre>
	 * uncapitalize(null)  = null
	 * uncapitalize("")    = ""
	 * uncapitalize("Cat") = "cat"
	 * uncapitalize("CAT") = "cAT"
	 * </pre>
	 *
	 * @see #capitalize(String)
	 *
	 * @param str 文字列
	 * @return 先頭を小文字にした文字列
	 */
	public static String uncapitalize(final String str) {
		if (str == null) {
			return null;
		} else if ("".equals(str)) {
			return "";
		} else {
			if (str.length() == 1) {
				return str.toLowerCase();
			} else {
				return str.substring(0, 1).toLowerCase() + str.substring(1);
			}
		}
	}

	/**
	 * 対象文字列がnull、空文字、空白のいずれかであること判定する
	 *
	 * <pre>
	 * isBlank(null)      = true
	 * isBlank("")        = true
	 * isBlank(" ")       = true
	 * isBlank("bob")     = false
	 * isBlank("  bob  ") = false
	 * </pre>
	 *
	 * @param cs 対象文字列
	 * @return null, 空文字もしくは空白の場合<code>true</code>
	 */
	public static boolean isBlank(final CharSequence cs) {
		if (isEmpty(cs)) {
			return true;
		}
		var len = cs.length();
		for (var i = 0; i < len; i++) {
			if (!Character.isWhitespace(cs.charAt(i))) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 対象文字列がnull、空文字、空白のいずれでもないこと判定する
	 *
	 * <pre>
	 * isNotBlank(null)      = false
	 * isNotBlank("")        = false
	 * isNotBlank(" ")       = false
	 * isNotBlank("bob")     = true
	 * isNotBlank("  bob  ") = true
	 * </pre>
	 *
	 * @see StringUtils#isNotBlank(CharSequence)
	 *
	 * @param cs 対象文字列
	 * @return null、空文字、空白以外の場合<code>true</code>
	 */
	public static boolean isNotBlank(final CharSequence cs) {
		return !isBlank(cs);
	}

	/**
	 * 対象文字列が数字だけで構成されていることを判定する.<br>
	 * 空文字や+-が含まれる場合は<code>false</code>を返す.
	 *
	 * <pre>
	 * isNumeric(null)   = false
	 * isNumeric("")     = false
	 * isNumeric("  ")   = false
	 * isNumeric("123")  = true
	 * isNumeric("\u0967\u0968\u0969")  = true
	 * isNumeric("12 3") = false
	 * isNumeric("ab2c") = false
	 * isNumeric("12-3") = false
	 * isNumeric("12.3") = false
	 * isNumeric("-123") = false
	 * isNumeric("+123") = false
	 * </pre>
	 *
	 * @param cs 判定対象文字列. またはnull
	 * @return 数字のみで構成される場合<code>true</code>
	 */
	public static boolean isNumeric(final CharSequence cs) {
		if (isEmpty(cs)) {
			return false;
		}
		for (var i = 0; i < cs.length(); i++) {
			if (!Character.isDigit(cs.charAt(i))) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 文字列の先頭から指定した文字数の文字列を取得する
	 *
	 * <pre>
	 * left(null, *)    = null
	 * left(*, -ve)     = ""
	 * left("", *)      = ""
	 * left("abc", 0)   = ""
	 * left("abc", 2)   = "ab"
	 * left("abc", 4)   = "abc"
	 * </pre>
	 *
	 * @param str 対象文字列
	 * @param len 文字数
	 * @return 文字列の先頭から文字数で指定した長さの文字列
	 */
	public static String left(final String str, final int len) {
		if (str == null) {
			return null;
		} else if (len < 0) {
			return "";
		} else if (str.length() <= len) {
			return str;
		} else {
			return str.substring(0, len);
		}
	}

	/**
	 * 文字列の左側に空白(' ')を埋める.<br>
	 *
	 * <pre>
	 * leftPad(null, *)   = null
	 * leftPad("", 3)     = "   "
	 * leftPad("bat", 3)  = "bat"
	 * leftPad("bat", 5)  = "  bat"
	 * leftPad("bat", 1)  = "bat"
	 * leftPad("bat", -1) = "bat"
	 * </pre>
	 *
	 * @param str 文字埋めする文字列、またはnull
	 * @param size 文字埋め後の長さ
	 * @return 文字埋めした文字列
	 */
	public static String leftPad(final String str, final int size) {
		return leftPad(str, size, ' ');
	}

	/**
	 * 文字列の左側に指定した埋め文字を埋める.<br>
	 *
	 * <pre>
	 * leftPad(null, *, *)     = null
	 * leftPad("", 3, 'z')     = "zzz"
	 * leftPad("bat", 3, 'z')  = "bat"
	 * leftPad("bat", 5, 'z')  = "zzbat"
	 * leftPad("bat", 1, 'z')  = "bat"
	 * leftPad("bat", -1, 'z') = "bat"
	 * </pre>
	 *
	 * @param str 文字埋めする文字列、またはnull
	 * @param size 文字埋め後の長さ
	 * @param padChar  文字埋めする文字
	 * @return 文字埋めした文字列
	 */
	public static String leftPad(final String str, final int size, final char padChar) {
		if (str == null) {
			return null;
		}
		var pads = size - str.length();
		if (pads <= 0) {
			return str;
		}
		return repeat(padChar, pads).concat(str);
	}

	/**
	 * 文字列の左側に指定した埋め文字を埋める.<br>
	 *
	 * <pre>
	 * leftPad(null, *, *)      = null
	 * leftPad("", 3, "z")      = "zzz"
	 * leftPad("bat", 3, "yz")  = "bat"
	 * leftPad("bat", 5, "yz")  = "yzbat"
	 * leftPad("bat", 8, "yz")  = "yzyzybat"
	 * leftPad("bat", 1, "yz")  = "bat"
	 * leftPad("bat", -1, "yz") = "bat"
	 * leftPad("bat", 5, null)  = "  bat"
	 * leftPad("bat", 5, "")    = "  bat"
	 * </pre>
	 *
	 * @param str 文字埋めする文字列、またはnull
	 * @param size 文字埋め後の長さ
	 * @param padStr  文字埋めする文字列
	 * @return 文字埋めした文字列
	 */
	public static String leftPad(final String str, final int size, String padStr) {
		if (str == null) {
			return null;
		}
		if (isEmpty(padStr)) {
			padStr = " ";
		}
		var padLen = padStr.length();
		var strLen = str.length();
		var pads = size - strLen;
		if (pads <= 0) {
			return str;
		}
		if (pads == padLen) {
			return padStr.concat(str);
		} else if (pads < padLen) {
			return padStr.substring(0, pads).concat(str);
		} else {
			var padding = new char[pads];
			var padChars = padStr.toCharArray();
			for (var i = 0; i < pads; i++) {
				padding[i] = padChars[i % padLen];
			}
			return new String(padding).concat(str);
		}
	}

	/**
	 * 文字列の指定した位置から指定した文字数の文字列を取得する.
	 *
	 * <pre>
	 * mid(null, *, *)    = null
	 * mid(*, *, -ve)     = ""
	 * mid("", 0, *)      = ""
	 * mid("abc", 0, 2)   = "ab"
	 * mid("abc", 0, 4)   = "abc"
	 * mid("abc", 2, 4)   = "c"
	 * mid("abc", 4, 2)   = ""
	 * mid("abc", -2, 2)  = "ab"
	 * </pre>
	 *
	 * @param str 対象文字列
	 * @param pos 開始位置
	 * @param len 文字数
	 * @return 文字列の開始位置から文字数で指定した長さの文字列
	 */
	public static String mid(final String str, final int pos, final int len) {
		if (str == null) {
			return null;
		} else if (len < 0 || pos > str.length()) {
			return "";
		}
		var newPos = pos < 0 ? 0 : pos;
		if (str.length() <= newPos + len) {
			return str.substring(newPos);
		} else {
			return str.substring(newPos, newPos + len);
		}
	}

	/**
	 * 末尾に削除する文字列がある場合のみ末尾から削除する.
	 *
	 * <pre>
	 * removeEnd(null, *)      = null
	 * removeEnd("", *)        = ""
	 * removeEnd(*, null)      = *
	 * removeEnd("www.domain.com", ".com.")  = "www.domain.com"
	 * removeEnd("www.domain.com", ".com")   = "www.domain"
	 * removeEnd("www.domain.com", "domain") = "www.domain.com"
	 * removeEnd("abc", "")    = "abc"
	 * </pre>
	 *
	 * @param str 削除対象文字列、もしくはnull
	 * @param remove  削除する末尾の文字列、もしくはnull
	 * @return 末尾からremoveで指定した文字列を削除した文字列. removeで指定した文字列が末尾にない場合は元の文字列を返す.
	 */
	public static String removeEnd(final String str, final String remove) {
		if (isEmpty(str) || isEmpty(remove)) {
			return str;
		}
		if (str.endsWith(remove)) {
			return str.substring(0, str.length() - remove.length());
		} else {
			return str;
		}
	}

	/**
	 * 指定したキャラクタを繰り返した文字列を取得する.
	 *
	 * <pre>
	 * repeat('e', 0)  = ""
	 * repeat('e', 3)  = "eee"
	 * repeat('e', -2) = ""
	 * </pre>
	 *
	 * @param ch 繰り返すキャラクタ
	 * @param repeat 繰り返し回数. 0またはマイナスも許容するがその場合0回と扱われる.
	 * @return キャラクタを指定回数繰り返した文字列
	 */
	public static String repeat(final char ch, final int repeat) {
		if (repeat <= 0) {
			return "";
		}
		var buf = new char[repeat];
		Arrays.fill(buf, ch);
		return new String(buf);
	}

	/**
	 * 文字列の最後から指定した文字数の文字列を取得する
	 *
	 * <pre>
	 * right(null, *)    = null
	 * right(*, -ve)     = ""
	 * right("", *)      = ""
	 * right("abc", 0)   = ""
	 * right("abc", 2)   = "bc"
	 * right("abc", 4)   = "abc"
	 * </pre>
	 *
	 * @param str 対象文字列
	 * @param len 文字数
	 * @return 文字列の最後から文字数で指定した長さの文字列
	 */
	public static String right(final String str, final int len) {
		if (str == null) {
			return null;
		} else if (len < 0) {
			return "";
		} else if (str.length() <= len) {
			return str;
		} else {
			return str.substring(str.length() - len);
		}
	}

	/**
	 * 文字列の右側に空白(' ')を埋める.<br>
	 *
	 * <pre>
	 * rightPad(null, *)   = null
	 * rightPad("", 3)     = "   "
	 * rightPad("bat", 3)  = "bat"
	 * rightPad("bat", 5)  = "bat  "
	 * rightPad("bat", 1)  = "bat"
	 * rightPad("bat", -1) = "bat"
	 * </pre>
	 *
	 * @param str 文字埋めする文字列、またはnull
	 * @param size 文字埋め後の長さ
	 * @return 文字埋めした文字列
	 */
	public static String rightPad(final String str, final int size) {
		return rightPad(str, size, ' ');
	}

	/**
	 * 文字列の右側に指定した埋め文字を埋める.<br>
	 *
	 * <pre>
	 * rightPad(null, *, *)     = null
	 * rightPad("", 3, 'z')     = "zzz"
	 * rightPad("bat", 3, 'z')  = "bat"
	 * rightPad("bat", 5, 'z')  = "batzz"
	 * rightPad("bat", 1, 'z')  = "bat"
	 * rightPad("bat", -1, 'z') = "bat"
	 * </pre>
	 *
	 * @param str 文字埋めする文字列、またはnull
	 * @param size 文字埋め後の長さ
	 * @param padChar  文字埋めする文字
	 * @return 文字埋めした文字列
	 */
	public static String rightPad(final String str, final int size, final char padChar) {
		if (str == null) {
			return null;
		}
		var pads = size - str.length();
		if (pads <= 0) {
			return str;
		}
		return str.concat(repeat(padChar, pads));
	}

	/**
	 * 文字列の右側に指定した埋め文字を埋める.<br>
	 *
	 * <pre>
	 * rightPad(null, *, *)      = null
	 * rightPad("", 3, "z")      = "zzz"
	 * rightPad("bat", 3, "yz")  = "bat"
	 * rightPad("bat", 5, "yz")  = "batyz"
	 * rightPad("bat", 8, "yz")  = "batyzyzy"
	 * rightPad("bat", 1, "yz")  = "bat"
	 * rightPad("bat", -1, "yz") = "bat"
	 * rightPad("bat", 5, null)  = "bat  "
	 * rightPad("bat", 5, "")    = "bat  "
	 * </pre>
	 *
	 * @param str 文字埋めする文字列、またはnull
	 * @param size 文字埋め後の長さ
	 * @param padStr  文字埋めする文字列
	 * @return 文字埋めした文字列
	 */
	public static String rightPad(final String str, final int size, String padStr) {
		if (str == null) {
			return null;
		}
		if (isEmpty(padStr)) {
			padStr = " ";
		}
		var padLen = padStr.length();
		var strLen = str.length();
		var pads = size - strLen;
		if (pads <= 0) {
			return str;
		}
		if (pads == padLen) {
			return str.concat(padStr);
		} else if (pads < padLen) {
			return str.concat(padStr.substring(0, pads));
		} else {
			var padding = new char[pads];
			var padChars = padStr.toCharArray();
			for (var i = 0; i < pads; i++) {
				padding[i] = padChars[i % padLen];
			}
			return str.concat(new String(padding));
		}
	}

	/**
	 * 文字列を空白で区切って配列に格納します。
	 * "空白"の判定には {@link Character#isWhitespace(char)} を使用します。
	 * {@code null} が入力された場合は {@code null} を返します
	 *
	 * <pre>
	 * split(null)       = null
	 * split("")         = []
	 * split("abc def")  = ["abc", "def"]
	 * split("abc  def") = ["abc", "def"]
	 * split(" abc ")    = ["abc"]
	 * </pre>
	 *
	 * @param str  変換元文字列 または {@code null}
	 * @return 空白で区切った文字列配列
	 */
	public static String[] split(final String str) {
		return split(str, null, -1);
	}

	/**
	 * 文字列を指定した区切り文字で区切って配列に格納します。
	 * {@code null} が入力された場合は {@code null} を返します
	 *
	 * <pre>
	 * split(null, *)         = null
	 * split("", *)           = []
	 * split("a.b.c", '.')    = ["a", "b", "c"]
	 * split("a..b.c", '.')   = ["a", "b", "c"]
	 * split("a:b:c", '.')    = ["a:b:c"]
	 * split("a b c", ' ')    = ["a", "b", "c"]
	 * </pre>
	 *
	 * @param str 変換元文字列 または {@code null}
	 * @param separatorChar 区切り文字
	 * @return 区切り文字で区切った文字列配列
	 */
	public static String[] split(final String str, final char separatorChar) {
		return split(str, String.valueOf(separatorChar), -1);
	}

	/**
	 * 文字列を指定した区切り文字で区切って配列に格納します。
	 * {@code null} が入力された場合は {@code null} を返します
	 *
	 * <pre>
	 * split(null, *, *)            = null
	 * split("", *, *)              = []
	 * split("ab cd ef", null, 0)   = ["ab", "cd", "ef"]
	 * split("ab   cd ef", null, 0) = ["ab", "cd", "ef"]
	 * split("ab:cd:ef", ":", 0)    = ["ab", "cd", "ef"]
	 * split("ab:cd:ef", ":", 2)    = ["ab", "cd:ef"]
	 * </pre>
	 *
	 * @param str 変換元文字列 または {@code null}
	 * @param separatorChars 区切り文字
	 * @param max 作成する配列の最大値。最大値を超える場合は最後の要素に残りのすべての文字列が入る
	 * @return 区切り文字で区切った文字列配列
	 */
	public static String[] split(final String str, final String separatorChars, final int max) {
		if (str == null) {
			return null;
		}
		if ("".equals(str)) {
			return new String[0];
		}

		var separator = separatorChars == null ? "\\s+" : Pattern.quote(separatorChars) + "+";
		return str.trim().split(separator, max);
	}

	/**
	 * 文字列の前後の空白を除去する。nullを渡した場合は結果もnullとなる
	 *
	 * <pre>
	 * trim(null)          = null
	 * trim("")            = ""
	 * trim("     ")       = ""
	 * trim("abc")         = "abc"
	 * trim("    abc    ") = "abc"
	 * </pre>
	 *
	 * @param str トリムする文字列
	 * @return トリム後の文字列。入力が<code>null</code>の場合は<code>null</code>
	 */
	public static String trim(final String str) {
		return str == null ? null : str.trim();
	}

	/**
	 * <p>
	 * 文字列の前後の空白を除去する。nullを渡した場合は空文字となる
	 *
	 * <pre>
	 * trimToEmpty(null)          = ""
	 * trimToEmpty("")            = ""
	 * trimToEmpty("     ")       = ""
	 * trimToEmpty("abc")         = "abc"
	 * trimToEmpty("    abc    ") = "abc"
	 * </pre>
	 *
	 * @param str トリムする文字列
	 * @return トリム後の文字列。入力が<code>null</code>の場合は空文字となる
	 */
	public static String trimToEmpty(final String str) {
		return str == null ? "" : str.trim();
	}

}
