/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.utils;

import jp.co.future.uroborosql.dialect.Dialect;

/**
 * OGNL式内で{@link StringUtils}
 * の提供するメソッドを利用するためにstaticメソッドをインスタンスメソッドとしてデリゲートし提供するクラス.<br>
 *
 * 利用する際はstatic final なフィールドに格納するなどして複数のインスタンスを生成しないようにすること
 *
 * @author H.Sugimoto
 */
public final class StringFunction {
	/** OGNL式から呼び出す際の略称名 {@value} */
	public static final String SHORT_NAME = "SF";

	/** dialect */
	private final Dialect dialect;

	/**
	 * コンストラクタ
	 */
	public StringFunction() {
		this(null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param dialect Dialect
	 */
	public StringFunction(final Dialect dialect) {
		super();
		this.dialect = dialect;
	}

	/**
	 * 対象文字列が空文字であること判定する
	 *
	 * <pre>
	 * isEmpty(null)      = true
	 * isEmpty("")        = true
	 * isEmpty(" ")       = false
	 * isEmpty("bob")     = false
	 * isEmpty("  bob  ") = false
	 * </pre>
	 *
	 * @see StringUtils#isEmpty(CharSequence)
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
	 * @param str 対象文字列
	 * @return 空文字でない場合<code>true</code>
	 */
	public boolean isNotEmpty(final String str) {
		return StringUtils.isNotEmpty(str);
	}

	/**
	 * 対象文字列が空文字、もしくは空白であること判定する
	 *
	 * <pre>
	 * isBlank(null)      = true
	 * isBlank("")        = true
	 * isBlank(" ")       = true
	 * isBlank("bob")     = false
	 * isBlank("  bob  ") = false
	 * </pre>
	 *
	 * @see StringUtils#isBlank(CharSequence)
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
	 * trim(null)          = null
	 * trim("")            = ""
	 * trim("     ")       = ""
	 * trim("abc")         = "abc"
	 * trim("    abc    ") = "abc"
	 * </pre>
	 *
	 * @see StringUtils#trim(String)
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
	 * trimToEmpty(null)          = ""
	 * trimToEmpty("")            = ""
	 * trimToEmpty("     ")       = ""
	 * trimToEmpty("abc")         = "abc"
	 * trimToEmpty("    abc    ") = "abc"
	 * </pre>
	 *
	 * @see StringUtils#trimToEmpty(String)
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
	 * left(null, *)    = null
	 * left(*, -ve)     = ""
	 * left("", *)      = ""
	 * left("abc", 0)   = ""
	 * left("abc", 2)   = "ab"
	 * left("abc", 4)   = "abc"
	 * </pre>
	 *
	 * @see StringUtils#left(String, int)
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
	 * right(null, *)    = null
	 * right(*, -ve)     = ""
	 * right("", *)      = ""
	 * right("abc", 0)   = ""
	 * right("abc", 2)   = "bc"
	 * right("abc", 4)   = "abc"
	 * </pre>
	 *
	 * @see StringUtils#right(String, int)
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
	 * @see StringUtils#mid(String, int, int)
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
	 * rightPad(null, *)   = null
	 * rightPad("", 3)     = "   "
	 * rightPad("bat", 3)  = "bat"
	 * rightPad("bat", 5)  = "bat  "
	 * rightPad("bat", 1)  = "bat"
	 * rightPad("bat", -1) = "bat"
	 * </pre>
	 *
	 * @see StringUtils#rightPad(String, int)
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
	 * rightPad(null, *, *)     = null
	 * rightPad("", 3, 'z')     = "zzz"
	 * rightPad("bat", 3, 'z')  = "bat"
	 * rightPad("bat", 5, 'z')  = "batzz"
	 * rightPad("bat", 1, 'z')  = "bat"
	 * rightPad("bat", -1, 'z') = "bat"
	 * </pre>
	 *
	 * @see StringUtils#rightPad(String, int, char)
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
	 * leftPad(null, *)   = null
	 * leftPad("", 3)     = "   "
	 * leftPad("bat", 3)  = "bat"
	 * leftPad("bat", 5)  = "  bat"
	 * leftPad("bat", 1)  = "bat"
	 * leftPad("bat", -1) = "bat"
	 * </pre>
	 *
	 * @see StringUtils#leftPad(String, int)
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
	 * leftPad(null, *, *)     = null
	 * leftPad("", 3, 'z')     = "zzz"
	 * leftPad("bat", 3, 'z')  = "bat"
	 * leftPad("bat", 5, 'z')  = "zzbat"
	 * leftPad("bat", 1, 'z')  = "bat"
	 * leftPad("bat", -1, 'z') = "bat"
	 * </pre>
	 *
	 * @see StringUtils#leftPad(String, int, char)
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
	 * @see StringUtils#split(String)
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
	 * @see StringUtils#split(String, char)
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
	 * @see StringUtils#split(String, String, int)
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
	 * capitalize(null)  = null
	 * capitalize("")    = ""
	 * capitalize("cat") = "Cat"
	 * capitalize("cAt") = "CAt"
	 * </pre>
	 *
	 * @see StringUtils#capitalize(String)
	 * @see #uncapitalize(String)
	 *
	 * @param str 文字列
	 * @return 先頭を大文字にした文字列
	 */
	public String capitalize(final String str) {
		return StringUtils.capitalize(str);
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
	 * @see StringUtils#uncapitalize(String)
	 * @see #capitalize(String)
	 *
	 * @param str 文字列
	 * @return 先頭を小文字にした文字列
	 */
	public String uncapitalize(final String str) {
		return StringUtils.uncapitalize(str);
	}

	/**
	 * 指定されたテキストで始まるLIKE句用の検索文字列を生成する。<br>
	 * 引数のテキストはエスケープ処理される。<br>
	 *
	 * <pre>
	 * ex)
	 *  - abc  -&gt; abc%
	 *  - a_bc -&gt; a$_bc%   (escape with '\' (oracle) or '$' (other) )
	 *  - ''   -&gt; %
	 *  - null -&gt; %
	 * </pre>
	 *
	 * @param text テキスト
	 * @return 指定されたテキストで始まるLIKE句用の検索文字列
	 */
	public String startsWith(final CharSequence text) {
		if (dialect == null) {
			throw new IllegalStateException("dialect is not set.");
		}
		if (StringUtils.isEmpty(text)) {
			return "%";
		} else {
			return dialect.escapeLikePattern(text) + "%";
		}
	}

	/**
	 * 指定されたテキストを含むLIKE句用の検索文字列を生成する。<br>
	 * 引数のテキストはエスケープ処理される。
	 *
	 * <pre>
	 * ex)
	 *  - abc  -&gt; %abc%
	 *  - a_bc -&gt; %a$_bc%   (escape with '\' (oracle) or '$' (other) )
	 *  - ''   -&gt; %
	 *  - null -&gt; %
	 * </pre>
	 *
	 * @param text テキスト
	 * @return 指定されたテキストを含むLIKE句用の検索文字列
	 */
	public String contains(final CharSequence text) {
		if (dialect == null) {
			throw new IllegalStateException("dialect is not set.");
		}
		if (StringUtils.isEmpty(text)) {
			return "%";
		} else {
			return "%" + dialect.escapeLikePattern(text) + "%";
		}
	}

	/**
	 * 指定されたテキストで終わるLIKE句用の検索文字列を生成する。<br>
	 * 引数のテキストはエスケープ処理される。
	 *
	 * <pre>
	 * ex)
	 *  - abc  -&gt; %abc
	 *  - a_bc -&gt; %a$_bc   (escape with '\' (oracle) or '$' (other) )
	 *  - ''   -&gt; %
	 *  - null -&gt; %
	 * </pre>
	 *
	 * @param text テキスト
	 * @return 指定されたテキストで終わるLIKE句用の検索文字列
	 */
	public String endsWith(final CharSequence text) {
		if (dialect == null) {
			throw new IllegalStateException("dialect is not set.");
		}
		if (StringUtils.isEmpty(text)) {
			return "%";
		} else {
			return "%" + dialect.escapeLikePattern(text);
		}
	}

	/**
	 * 加算を行う. 最大値に対して加算した場合はオーバーフローする.<br>
	 * 引数の型（short/int/longのいづれか）に合わせて計算を行う.
	 *
	 * @param num 加算を行う数値
	 * @return 加算後の数値（オーバーフローした場合は引数の型の最小値になる）
	 */
	public Object increment(final Object num) {
		if (num instanceof Short) {
			return incrementShort((Short) num);
		} else if (num instanceof Integer) {
			return incrementInt((Integer) num);
		} else if (num instanceof Long) {
			return incrementLong((Long) num);
		} else {
			throw new IllegalArgumentException("Invalid value type. num must be of type short/int/long.");
		}
	}

	/**
	 * short型で加算を行う. 最大値に対して加算した場合はオーバーフローする.
	 *
	 * @param num 加算を行う数値
	 * @return 加算後の数値（オーバーフローした場合はshort型の最小値になる）
	 */
	public short incrementShort(final short num) {
		return (short) (num + 1);
	}

	/**
	 * int型で加算を行う. 最大値に対して加算した場合はオーバーフローする.
	 *
	 * @param num 加算を行う数値
	 * @return 加算後の数値（オーバーフローした場合はint型の最小値になる）
	 */
	public int incrementInt(final int num) {
		return num + 1;
	}

	/**
	 * long型で加算を行う. 最大値に対して加算した場合はオーバーフローする.
	 *
	 * @param num 加算を行う数値
	 * @return 加算後の数値（オーバーフローした場合はlong型の最小値になる）
	 */
	public long incrementLong(final long num) {
		return num + 1L;
	}

}
