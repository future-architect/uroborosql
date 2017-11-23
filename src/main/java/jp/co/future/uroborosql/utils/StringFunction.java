package jp.co.future.uroborosql.utils;

import org.apache.commons.lang3.StringUtils;

/**
 * OGNL式内で{@link StringUtils}
 * の提供するメソッドを利用するためにstaticメソッドをインスタンスメソッドとしてデリゲートし提供するクラス.<br>
 *
 * 利用する際はstatic final なフィールドに格納するなどして複数のインスタンスを生成しないようにすること
 *
 * @author H.Sugimoto
 */
public class StringFunction {
	/** OGNL式から呼び出す際の略称名 {@value} */
	public static final String SHORT_NAME = "SF";

	/**
	 * コンストラクタ
	 */
	public StringFunction() {
		super();
	}

	/**
	 * 対象文字列が空文字であること判定する
	 *
	 * @see org.apache.commons.lang3.StringUtils#isEmpty(CharSequence)
	 *
	 * @param str 対象文字列
	 * @return 空文字の場合<code>true</code>
	 */
	public boolean isEmpty(final String str) {
		return StringUtils.isEmpty(str);
	}

	/**
	 * 対象文字列が空文字でないことを判定する
	 *
	 * @see org.apache.commons.lang3.StringUtils#isNotEmpty(CharSequence)
	 *
	 * @param str 対象文字列
	 * @return 空文字でない場合<code>true</code>
	 */
	public boolean isNotEmpty(final String str) {
		return StringUtils.isNotEmpty(str);
	}

	/**
	 * 対象文字列が空文字、もしくは空白であること判定する
	 *
	 * @see org.apache.commons.lang3.StringUtils#isBlank(CharSequence)
	 *
	 * @param str 対象文字列
	 * @return 空文字もしくは空白の場合<code>true</code>
	 */
	public boolean isBlank(final String str) {
		return StringUtils.isBlank(str);
	}

	/**
	 * 対象文字列が空文字、もしくは空白でないこと判定する
	 *
	 * @see org.apache.commons.lang3.StringUtils#isBlank(CharSequence)
	 *
	 * @param str 対象文字列
	 * @return 空文字もしくは空白以外の場合<code>true</code>
	 */
	public boolean isNotBlank(final String str) {
		return StringUtils.isNotBlank(str);
	}

	/**
	 * 文字列の前後の空白を除去する。nullを渡した場合は結果もnullとなる
	 *
	 * <pre>
	 * StringUtils.trim(null)          = null
	 * StringUtils.trim("")            = ""
	 * StringUtils.trim("     ")       = ""
	 * StringUtils.trim("abc")         = "abc"
	 * StringUtils.trim("    abc    ") = "abc"
	 * </pre>
	 *
	 * @param str トリムする文字列
	 * @return トリム後の文字列。入力が<code>null</code>の場合は<code>null</code>
	 */
	public String trim(final String str) {
		return StringUtils.trim(str);
	}

	/**
	 * <p>
	 * 文字列の前後の空白を除去する。nullを渡した場合は空文字となる
	 *
	 * <pre>
	 * StringUtils.trimToEmpty(null)          = ""
	 * StringUtils.trimToEmpty("")            = ""
	 * StringUtils.trimToEmpty("     ")       = ""
	 * StringUtils.trimToEmpty("abc")         = "abc"
	 * StringUtils.trimToEmpty("    abc    ") = "abc"
	 * </pre>
	 *
	 * @param str トリムする文字列
	 * @return トリム後の文字列。入力が<code>null</code>の場合は空文字となる
	 */
	public String trimToEmpty(final String str) {
		return StringUtils.trimToEmpty(str);
	}

	/**
	 * 文字列の先頭から指定した文字数の文字列を取得する
	 *
	 * <pre>
	 * StringUtils.left(null, *)    = null
	 * StringUtils.left(*, -ve)     = ""
	 * StringUtils.left("", *)      = ""
	 * StringUtils.left("abc", 0)   = ""
	 * StringUtils.left("abc", 2)   = "ab"
	 * StringUtils.left("abc", 4)   = "abc"
	 * </pre>
	 *
	 * @param str 対象文字列
	 * @param len 文字数
	 * @return 文字列の先頭から文字数で指定した長さの文字列
	 */
	public String left(final String str, final int len) {
		return StringUtils.left(str, len);
	}

	/**
	 * 文字列の最後から指定した文字数の文字列を取得する
	 *
	 * <pre>
	 * StringUtils.right(null, *)    = null
	 * StringUtils.right(*, -ve)     = ""
	 * StringUtils.right("", *)      = ""
	 * StringUtils.right("abc", 0)   = ""
	 * StringUtils.right("abc", 2)   = "bc"
	 * StringUtils.right("abc", 4)   = "abc"
	 * </pre>
	 *
	 * @param str 対象文字列
	 * @param len 文字数
	 * @return 文字列の最後から文字数で指定した長さの文字列
	 */
	public String right(final String str, final int len) {
		return StringUtils.right(str, len);
	}

	/**
	 * 文字列の指定した位置から指定した文字数の文字列を取得する
	 *
	 * <pre>
	 * StringUtils.mid(null, *, *)    = null
	 * StringUtils.mid(*, *, -ve)     = ""
	 * StringUtils.mid("", 0, *)      = ""
	 * StringUtils.mid("abc", 0, 2)   = "ab"
	 * StringUtils.mid("abc", 0, 4)   = "abc"
	 * StringUtils.mid("abc", 2, 4)   = "c"
	 * StringUtils.mid("abc", 4, 2)   = ""
	 * StringUtils.mid("abc", -2, 2)  = "ab"
	 * </pre>
	 *
	 * @param str 対象文字列
	 * @param pos 開始位置
	 * @param len 文字数
	 * @return 文字列の開始位置から文字数で指定した長さの文字列
	 */
	public String mid(final String str, final int pos, final int len) {
		return StringUtils.mid(str, pos, len);
	}

	/**
	 * 文字列の末尾に空白を埋めて指定された長さにする
	 *
	 * <pre>
	 * StringUtils.rightPad(null, *)   = null
	 * StringUtils.rightPad("", 3)     = "   "
	 * StringUtils.rightPad("bat", 3)  = "bat"
	 * StringUtils.rightPad("bat", 5)  = "bat  "
	 * StringUtils.rightPad("bat", 1)  = "bat"
	 * StringUtils.rightPad("bat", -1) = "bat"
	 * </pre>
	 *
	 * @param str 文字列
	 * @param size 文字埋め後の長さ
	 * @return 指定した長さになるまで末尾に空白を埋めた文字列
	 */
	public String rightPad(final String str, final int size) {
		return StringUtils.rightPad(str, size);
	}

	/**
	 * 文字列の末尾に指定した埋め込み文字を埋めて指定された長さにする
	 *
	 * <pre>
	 * StringUtils.rightPad(null, *, *)     = null
	 * StringUtils.rightPad("", 3, 'z')     = "zzz"
	 * StringUtils.rightPad("bat", 3, 'z')  = "bat"
	 * StringUtils.rightPad("bat", 5, 'z')  = "batzz"
	 * StringUtils.rightPad("bat", 1, 'z')  = "bat"
	 * StringUtils.rightPad("bat", -1, 'z') = "bat"
	 * </pre>
	 *
	 * @param str 文字列
	 * @param size 文字埋め後の長さ
	 * @param padChar 埋め込み文字
	 * @return 指定した長さになるまで末尾に埋め込み文字を埋めた文字列
	 */
	public String rightPad(final String str, final int size, final char padChar) {
		return StringUtils.rightPad(str, size, padChar);
	}

	/**
	 * 文字列の先頭に空白を埋めて指定された長さにする
	 *
	 * <pre>
	 * StringUtils.leftPad(null, *)   = null
	 * StringUtils.leftPad("", 3)     = "   "
	 * StringUtils.leftPad("bat", 3)  = "bat"
	 * StringUtils.leftPad("bat", 5)  = "  bat"
	 * StringUtils.leftPad("bat", 1)  = "bat"
	 * StringUtils.leftPad("bat", -1) = "bat"
	 * </pre>
	 *
	 * @param str 文字列
	 * @param size 文字埋め後の長さ
	 * @return 指定した長さになるまで先頭に空白を埋めた文字列
	 */
	public String leftPad(final String str, final int size) {
		return StringUtils.leftPad(str, size);
	}

	/**
	 * 文字列の先頭に指定した埋め込み文字を埋めて指定された長さにする
	 *
	 * <pre>
	 * StringUtils.leftPad(null, *, *)     = null
	 * StringUtils.leftPad("", 3, 'z')     = "zzz"
	 * StringUtils.leftPad("bat", 3, 'z')  = "bat"
	 * StringUtils.leftPad("bat", 5, 'z')  = "zzbat"
	 * StringUtils.leftPad("bat", 1, 'z')  = "bat"
	 * StringUtils.leftPad("bat", -1, 'z') = "bat"
	 * </pre>
	 *
	 * @param str 文字列
	 * @param size 文字埋め後の長さ
	 * @param padChar 埋め込み文字
	 * @return 指定した長さになるまで末尾に埋め込み文字を埋めた文字列
	 */
	public String leftPad(final String str, final int size, final char padChar) {
		return StringUtils.leftPad(str, size, padChar);
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
	public String[] split(final String str) {
		return StringUtils.split(str);
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
	public String[] split(final String str, final char separatorChar) {
		return StringUtils.split(str, separatorChar);
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
	public String[] split(final String str, final String separatorChars, final int max) {
		return StringUtils.split(str, separatorChars, max);
	}

	/**
	 * 文字列の先頭文字を大文字にする
	 *
	 * <pre>
	 * StringUtils.capitalize(null)  = null
	 * StringUtils.capitalize("")    = ""
	 * StringUtils.capitalize("cat") = "Cat"
	 * StringUtils.capitalize("cAt") = "CAt"
	 * </pre>
	 *
	 * @param str 文字列
	 * @return 先頭を大文字にした文字列
	 * @see StringUtils#capitalize(String)
	 * @see #uncapitalize(String)
	 */
	public String capitalize(final String str) {
		return StringUtils.capitalize(str);
	}

	/**
	 * 文字列の先頭を小文字にする
	 *
	 * <pre>
	 * StringUtils.uncapitalize(null)  = null
	 * StringUtils.uncapitalize("")    = ""
	 * StringUtils.uncapitalize("Cat") = "cat"
	 * StringUtils.uncapitalize("CAT") = "cAT"
	 * </pre>
	 *
	 * @param str 文字列
	 * @return 先頭を小文字にした文字列
	 * @see StringUtils#uncapitalize(String)
	 * @see #capitalize(String)
	 */
	public String uncapitalize(final String str) {
		return StringUtils.uncapitalize(str);
	}

}
