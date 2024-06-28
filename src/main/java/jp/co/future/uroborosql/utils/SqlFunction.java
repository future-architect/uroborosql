/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.utils;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import jp.co.future.uroborosql.dialect.Dialect;

/**
 * 評価式内で利用するメソッドを提供するクラス.<br>
 * 評価式内で{@link ObjectUtils} の提供するメソッドを利用できるようにするためにstaticメソッドをインスタンスメソッドとしてデリゲートし提供する.<br>
 *
 * 利用する際はstatic final なフィールドに格納するなどして複数のインスタンスを生成しないようにすること
 *
 * @author H.Sugimoto
 */
public final class SqlFunction {
	/** 評価式から呼び出す際の略称名 {@value} */
	public static final String SHORT_NAME = "SF";

	/** dialect */
	private final Dialect dialect;

	/**
	 * コンストラクタ
	 */
	public SqlFunction() {
		this(null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param dialect Dialect
	 */
	public SqlFunction(final Dialect dialect) {
		this.dialect = dialect;
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
	 * @see ObjectUtils#isEmpty(Object)
	 * @see Optional#isEmpty()
	 * @see Collection#isEmpty()
	 * @see Map#isEmpty()
	 *
	 * @param obj 対象オブジェクト
	 * @return null / 空文字 / 空配列 / 空Collection / 空Map の場合<code>true</code>
	 */
	public boolean isEmpty(final Object obj) {
		return ObjectUtils.isEmpty(obj);
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
	 * @see ObjectUtils#isNotEmpty(Object)
	 * @see Optional#isEmpty()
	 * @see Collection#isEmpty()
	 * @see Map#isEmpty()
	 *
	 * @param obj 対象オブジェクト
	 * @return null / 空文字 / 空配列 / 空Collection / 空Map 以外の場合<code>true</code>
	 */
	public boolean isNotEmpty(final Object obj) {
		return ObjectUtils.isNotEmpty(obj);
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
	 * @see ObjectUtils#isBlank(CharSequence)
	 *
	 * @param obj 対象オブジェクト
	 * @return null または 空文字もしくは空白の場合<code>true</code>
	 */
	public boolean isBlank(final Object obj) {
		return ObjectUtils.isBlank(getStringValue(obj, ""));
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
	 * @see ObjectUtils#isNotBlank(CharSequence)
	 *
	 * @param obj 対象オブジェクト
	 * @return null、空文字、空白のいずれでもない場合<code>true</code>
	 */
	public boolean isNotBlank(final Object obj) {
		return ObjectUtils.isNotBlank(getStringValue(obj, ""));
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
	 * @see ObjectUtils#trim(String)
	 *
	 * @param obj トリムする文字列を表すオブジェクト
	 * @return トリム後の文字列。入力が<code>null</code>の場合は<code>null</code>
	 */
	public String trim(final Object obj) {
		return ObjectUtils.trim(getStringValue(obj));
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
	 * @see ObjectUtils#trimToEmpty(String)
	 *
	 * @param obj トリムする文字列を表すオブジェクト
	 * @return トリム後の文字列。入力が<code>null</code>の場合は空文字となる
	 */
	public String trimToEmpty(final Object obj) {
		return ObjectUtils.trimToEmpty(getStringValue(obj));
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
	 * @see ObjectUtils#left(String, int)
	 *
	 * @param obj 対象文字列を表すオブジェクト
	 * @param len 文字数
	 * @return 文字列の先頭から文字数で指定した長さの文字列
	 */
	public String left(final Object obj, final int len) {
		return ObjectUtils.left(getStringValue(obj), len);
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
	 * @see ObjectUtils#right(String, int)
	 *
	 * @param obj 対象文字列を表すオブジェクト
	 * @param len 文字数
	 * @return 文字列の最後から文字数で指定した長さの文字列
	 */
	public String right(final Object obj, final int len) {
		return ObjectUtils.right(getStringValue(obj), len);
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
	 * @see ObjectUtils#mid(String, int, int)
	 *
	 * @param obj 対象文字列を表すオブジェクト
	 * @param pos 開始位置
	 * @param len 文字数
	 * @return 文字列の開始位置から文字数で指定した長さの文字列
	 */
	public String mid(final Object obj, final int pos, final int len) {
		return ObjectUtils.mid(getStringValue(obj), pos, len);
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
	 * @see ObjectUtils#rightPad(String, int)
	 *
	 * @param obj 文字列を表すオブジェクト
	 * @param size 文字埋め後の長さ
	 * @return 指定した長さになるまで末尾に空白を埋めた文字列
	 */
	public String rightPad(final Object obj, final int size) {
		return ObjectUtils.rightPad(getStringValue(obj), size);
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
	 * @see ObjectUtils#rightPad(String, int, char)
	 *
	 * @param obj 文字列を表すオブジェクト
	 * @param size 文字埋め後の長さ
	 * @param padChar 埋め込み文字
	 * @return 指定した長さになるまで末尾に埋め込み文字を埋めた文字列
	 */
	public String rightPad(final Object obj, final int size, final char padChar) {
		return ObjectUtils.rightPad(getStringValue(obj), size, padChar);
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
	 * @see ObjectUtils#leftPad(String, int)
	 *
	 * @param obj 文字列を表すオブジェクト
	 * @param size 文字埋め後の長さ
	 * @return 指定した長さになるまで先頭に空白を埋めた文字列
	 */
	public String leftPad(final Object obj, final int size) {
		return ObjectUtils.leftPad(getStringValue(obj), size);
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
	 * @see ObjectUtils#leftPad(String, int, char)
	 *
	 * @param obj 文字列を表すオブジェクト
	 * @param size 文字埋め後の長さ
	 * @param padChar 埋め込み文字
	 * @return 指定した長さになるまで末尾に埋め込み文字を埋めた文字列
	 */
	public String leftPad(final Object obj, final int size, final char padChar) {
		return ObjectUtils.leftPad(getStringValue(obj), size, padChar);
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
	 * @see ObjectUtils#split(String)
	 *
	 * @param obj  変換元文字列を表すオブジェクト または {@code null}
	 * @return 空白で区切った文字列配列
	 */
	public String[] split(final Object obj) {
		return ObjectUtils.split(getStringValue(obj));
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
	 * @see ObjectUtils#split(String, char)
	 *
	 * @param obj 変換元文字列を表すオブジェクト または {@code null}
	 * @param separatorChar 区切り文字
	 * @return 区切り文字で区切った文字列配列
	 */
	public String[] split(final Object obj, final char separatorChar) {
		return ObjectUtils.split(getStringValue(obj), separatorChar);
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
	 * @see ObjectUtils#split(String, String, int)
	 *
	 * @param obj 変換元文字列を表すオブジェクト または {@code null}
	 * @param separatorChars 区切り文字
	 * @param max 作成する配列の最大値。最大値を超える場合は最後の要素に残りのすべての文字列が入る
	 * @return 区切り文字で区切った文字列配列
	 */
	public String[] split(final Object obj, final String separatorChars, final int max) {
		return ObjectUtils.split(getStringValue(obj), separatorChars, max);
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
	 * @see ObjectUtils#capitalize(String)
	 * @see #uncapitalize(Object)
	 *
	 * @param obj 文字列を表すオブジェクト
	 * @return 先頭を大文字にした文字列
	 */
	public String capitalize(final Object obj) {
		return ObjectUtils.capitalize(getStringValue(obj));
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
	 * @see ObjectUtils#uncapitalize(String)
	 * @see #capitalize(Object)
	 *
	 * @param obj 文字列を表すオブジェクト
	 * @return 先頭を小文字にした文字列
	 */
	public String uncapitalize(final Object obj) {
		return ObjectUtils.uncapitalize(getStringValue(obj));
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
	 * @param obj テキストを表すオブジェクト
	 * @return 指定されたテキストで始まるLIKE句用の検索文字列
	 */
	public String startsWith(final Object obj) {
		if (dialect == null) {
			throw new IllegalStateException("dialect is not set.");
		}
		CharSequence val = getStringValue(obj);
		if (ObjectUtils.isEmpty(val)) {
			return "%";
		} else {
			return dialect.escapeLikePattern(val) + "%";
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
	 * @param obj テキストを表すオブジェクト
	 * @return 指定されたテキストを含むLIKE句用の検索文字列
	 */
	public String contains(final Object obj) {
		if (dialect == null) {
			throw new IllegalStateException("dialect is not set.");
		}
		CharSequence val = getStringValue(obj);
		if (ObjectUtils.isEmpty(val)) {
			return "%";
		} else {
			return "%" + dialect.escapeLikePattern(val) + "%";
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
	 * @param obj テキストを表すオブジェクト
	 * @return 指定されたテキストで終わるLIKE句用の検索文字列
	 */
	public String endsWith(final Object obj) {
		if (dialect == null) {
			throw new IllegalStateException("dialect is not set.");
		}
		CharSequence val = getStringValue(obj);
		if (ObjectUtils.isEmpty(val)) {
			return "%";
		} else {
			return "%" + dialect.escapeLikePattern(val);
		}
	}

	/**
	 * 加算を行う. 最大値に対して加算した場合はオーバーフローする.<br>
	 * 引数の型（short/int/longのいずれか）に合わせて計算を行う.
	 *
	 * @param <T> 引数の型
	 * @param num 加算を行う数値
	 * @return 加算後の数値（オーバーフローした場合は引数の型の最小値になる）
	 */
	@SuppressWarnings("unchecked")
	public <T extends Number> T increment(final T num) {
		if (num instanceof Short) {
			return (T) incrementShort((Short) num);
		} else if (num instanceof Integer) {
			return (T) incrementInt((Integer) num);
		} else if (num instanceof Long) {
			return (T) incrementLong((Long) num);
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
	public Short incrementShort(final Short num) {
		return (short) (num + 1);
	}

	/**
	 * int型で加算を行う. 最大値に対して加算した場合はオーバーフローする.
	 *
	 * @param num 加算を行う数値
	 * @return 加算後の数値（オーバーフローした場合はint型の最小値になる）
	 */
	public Integer incrementInt(final Integer num) {
		return num + 1;
	}

	/**
	 * long型で加算を行う. 最大値に対して加算した場合はオーバーフローする.
	 *
	 * @param num 加算を行う数値
	 * @return 加算後の数値（オーバーフローした場合はlong型の最小値になる）
	 */
	public Long incrementLong(final Long num) {
		return num + 1L;
	}

	/**
	 * 現在時刻のTimestampを取得する.
	 *
	 * @param zoneId ZoneId. <code>null</code> の場合はシステムクロックが利用される.
	 * @return 現在時刻のTimestamp
	 */
	public Timestamp nowTimestamp(final String zoneId) {
		if (zoneId == null) {
			return Timestamp.valueOf(LocalDateTime.now());
		} else {
			return Timestamp.valueOf(LocalDateTime.now(ZoneId.of(zoneId)));
		}
	}

	/**
	 * 現在時刻のTimestampを取得する（ゾーン計算あり）.
	 *
	 * @param zoneId ZoneId. <code>null</code> の場合はシステムクロックが利用される.
	 * @return 現在時刻のTimestamp
	 */
	public Timestamp nowTimestampWithZone(final String zoneId) {
		if (zoneId == null) {
			return new Timestamp(ZonedDateTime.now().toInstant().toEpochMilli());
		} else {
			return new Timestamp(ZonedDateTime.now(ZoneId.of(zoneId)).toInstant().toEpochMilli());
		}
	}

	/**
	 * 引数のオブジェクトを文字列に変換した値を取得する.
	 *
	 * @param obj 変換対象オブジェクト
	 * @return オブジェクトの文字列表現
	 */
	private String getStringValue(final Object obj) {
		return getStringValue(obj, null);
	}

	/**
	 * 引数のオブジェクトを文字列に変換した値を取得する.
	 *
	 * @param obj 変換対象オブジェクト
	 * @param nullDefault objが<code>null</code>の場合の初期値
	 * @return オブジェクトの文字列表現
	 */
	private String getStringValue(final Object obj, final String nullDefault) {
		var val = obj instanceof Optional ? ((Optional<?>) obj).orElse(null) : obj;
		if (val instanceof String) {
			return String.class.cast(val);
		} else {
			return Objects.toString(val, nullDefault);
		}
	}

}
