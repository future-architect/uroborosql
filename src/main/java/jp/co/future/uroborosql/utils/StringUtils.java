/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.utils;

import java.util.Arrays;
import java.util.regex.Pattern;

public final class StringUtils {
	private StringUtils() {
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
		int len = cs.length();
		for (int i = 0; i < len; i++) {
			if (!Character.isWhitespace(cs.charAt(i))) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 対象文字列がnull または 空文字であること判定する
	 *
	 * <pre>
	 * isEmpty(null)      = true
	 * isEmpty("")        = true
	 * isEmpty(" ")       = false
	 * isEmpty("bob")     = false
	 * isEmpty("  bob  ") = false
	 * </pre>
	 *
	 * @param cs 対象文字列
	 * @return 空文字の場合<code>true</code>
	 */
	public static boolean isEmpty(final CharSequence cs) {
		return cs == null || cs.length() == 0;
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
	 * 対象文字列がnull または空文字でないことを判定する
	 *
	 * <pre>
	 * isNotEmpty(null)      = false
	 * isNotEmpty("")        = false
	 * isNotEmpty(" ")       = true
	 * isNotEmpty("bob")     = true
	 * isNotEmpty("  bob  ") = true
	 * </pre>
	 *
	 * @see StringUtils#isNotEmpty(CharSequence)
	 *
	 * @param cs 対象文字列
	 * @return null または空文字でない場合<code>true</code>
	 */
	public static boolean isNotEmpty(final CharSequence cs) {
		return !isEmpty(cs);
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
		for (int i = 0; i < cs.length(); i++) {
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
		final int pads = size - str.length();
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
		final int padLen = padStr.length();
		final int strLen = str.length();
		final int pads = size - strLen;
		if (pads <= 0) {
			return str;
		}
		if (pads == padLen) {
			return padStr.concat(str);
		} else if (pads < padLen) {
			return padStr.substring(0, pads).concat(str);
		} else {
			final char[] padding = new char[pads];
			final char[] padChars = padStr.toCharArray();
			for (int i = 0; i < pads; i++) {
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
		int newPos = pos < 0 ? 0 : pos;
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
		final char[] buf = new char[repeat];
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
		final int pads = size - str.length();
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
		final int padLen = padStr.length();
		final int strLen = str.length();
		final int pads = size - strLen;
		if (pads <= 0) {
			return str;
		}
		if (pads == padLen) {
			return str.concat(padStr);
		} else if (pads < padLen) {
			return str.concat(padStr.substring(0, pads));
		} else {
			final char[] padding = new char[pads];
			final char[] padChars = padStr.toCharArray();
			for (int i = 0; i < pads; i++) {
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

		String separator = separatorChars == null ? "\\s+" : Pattern.quote(separatorChars) + "+";
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
