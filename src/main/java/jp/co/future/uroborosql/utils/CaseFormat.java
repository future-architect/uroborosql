/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
/**
 *
 */
package jp.co.future.uroborosql.utils;

/** 文字列書式 */
public enum CaseFormat {
	/** パスカルケース（先頭大文字） */
	PASCAL_CASE {
		/**
		 * パスカル式文字列に変換する
		 *
		 * <pre>
		 * 	convert("CAMEL")    ⇒	"Camel"
		 * 	convert("TO_CAMEL") ⇒	"ToCamel"
		 * 	convert("")         ⇒	""
		 * 	convert(null)       ⇒	""
		 * </pre>
		 *
		 * @param original 変換元文字列
		 * @return パスカル式文字列
		 *
		 * @see jp.co.future.uroborosql.utils.CaseFormat#convert(java.lang.String)
		 */
		@Override
		public String convert(final String original) {
			if (original == null || "".equals(original)) {
				return "";
			}
			var str = original.trim();
			if (str.isEmpty()) {
				return str;
			}
			var builder = new StringBuilder();
			var len = str.length();

			if (!str.contains("_")) {
				if (isUpperCase(str)) {
					str = str.toLowerCase();
				}
				builder.append(Character.toUpperCase(str.charAt(0)));
				if (len > 1) {
					builder.append(str.substring(1));
				}
			} else {
				str = str.toLowerCase();
				var i = 0;
				while (i < len) {
					var ch = str.charAt(i);
					if (i == 0 && ('a' <= ch && ch <= 'z' || '0' <= ch && ch <= '9')) {
						builder.append(Character.toUpperCase(str.charAt(i)));
					} else if (ch == '_') {
						i++;
						if (i < len) {
							builder.append(Character.toUpperCase(str.charAt(i)));
						}
					} else {
						builder.append(ch);
					}
					i++;
				}
			}
			return builder.toString();
		}
	},

	/** キャメルケース（先頭小文字） */
	CAMEL_CASE {
		/**
		 * キャメル式文字列に変換する
		 *
		 * <pre>
		 * 	convert("CAMEL")    ⇒	"camel"
		 * 	convert("TO_CAMEL") ⇒	"toCamel"
		 * 	convert("")         ⇒	""
		 * 	convert(null)       ⇒	""
		 * </pre>
		 *
		 * @param original 変換元文字列
		 * @return キャメル式文字列
		 *
		 * @see jp.co.future.uroborosql.utils.CaseFormat#convert(java.lang.String)
		 */
		@Override
		public String convert(final String original) {
			if (original == null || "".equals(original)) {
				return "";
			}
			var str = original.trim();
			if (str.isEmpty()) {
				return str;
			}
			var len = str.length();
			if (!str.contains("_")) {
				var ch = str.charAt(0);
				if ('a' <= ch && ch <= 'z' || '0' <= ch && ch <= '9') {
					// 先頭小文字で"_"を含まない場合（つまりすでにCamelCaseの場合）、文字列変換せずにそのまま返す
					return str;
				} else if (!isUpperCase(str)) {
					var builder = new StringBuilder();
					builder.append(Character.toLowerCase(str.charAt(0)));
					if (len > 1) {
						builder.append(str.substring(1));
					}
					return builder.toString();
				}
			}
			var builder = new StringBuilder();
			str = str.toLowerCase();
			var i = 0;
			while (i < len) {
				var ch = str.charAt(i);
				if (ch == '_') {
					i++;
					if (i < len) {
						builder.append(Character.toUpperCase(str.charAt(i)));
					}
				} else {
					builder.append(ch);
				}
				i++;
			}
			return builder.toString();
		}
	},

	/** スネーク式文字列（大文字） */
	UPPER_SNAKE_CASE {
		/**
		 * スネーク式文字列（大文字）に変換する
		 *
		 * <pre>
		 * 	convert("snake")    ⇒	"SNAKE"
		 * 	convert("SNAKE")    ⇒	"SNAKE"
		 * 	convert("toSnake")  ⇒	"TO_SNAKE"
		 * 	convert("")         ⇒	""
		 * 	convert(null)       ⇒	""
		 * </pre>
		 *
		 * @param original 変換元文字列
		 * @return スネーク式文字列（大文字）
		 *
		 * @see jp.co.future.uroborosql.utils.CaseFormat#convert(java.lang.String)
		 */
		@Override
		public String convert(final String original) {
			if (original == null || "".equals(original)) {
				return "";
			}
			var str = original.trim();
			if (str.contains("_") || str.toUpperCase().equals(str)) {
				return str.toUpperCase();
			}

			var builder = new StringBuilder();
			for (var i = 0; i < str.length(); i++) {
				var ch = str.charAt(i);
				if ('a' <= ch && ch <= 'z' || '0' <= ch && ch <= '9') {
					builder.append(Character.toUpperCase(ch));
				} else if ('A' <= ch && ch <= 'Z') {
					if (i > 0) {
						builder.append('_');
					}
					builder.append(ch);
				} else if (ch == '_') {
					builder.append('_');
				}
				// does not support for multibyte characters
			}
			return builder.toString();
		}
	},
	/** スネーク式文字列（小文字） */
	LOWER_SNAKE_CASE {
		/**
		 * スネーク式文字列（小文字）に変換する
		 *
		 * <pre>
		 * 	convert("snake")    ⇒	"snake"
		 * 	convert("SNAKE")    ⇒	"snake"
		 * 	convert("toSnake")  ⇒	"to_snake"
		 * 	convert("")         ⇒	""
		 * 	convert(null)       ⇒	""
		 * </pre>
		 *
		 * @param original 変換元文字列
		 * @return スネーク式文字列（小文字）
		 *
		 * @see jp.co.future.uroborosql.utils.CaseFormat#convert(java.lang.String)
		 */
		@Override
		public String convert(final String original) {
			if (original == null || "".equals(original)) {
				return "";
			}
			var str = original.trim();
			if (str.contains("_") || str.toLowerCase().equals(str) || str.toUpperCase().equals(str)) {
				return str.toLowerCase();
			}

			var builder = new StringBuilder();
			for (var i = 0; i < str.length(); i++) {
				var ch = str.charAt(i);
				if ('a' <= ch && ch <= 'z' || '0' <= ch && ch <= '9') {
					builder.append(Character.toLowerCase(ch));
				} else if ('A' <= ch && ch <= 'Z') {
					if (i > 0) {
						builder.append('_');
					}
					builder.append(Character.toLowerCase(ch));
				} else if (ch == '_') {
					builder.append('_');
				}
				// does not support for multibyte characters
			}
			return builder.toString();
		}
	},

	/** 単純大文字化 */
	UPPER_CASE {

		@Override
		public String convert(final String original) {
			if (original == null || "".equals(original)) {
				return "";
			}
			return original.trim().toUpperCase();
		}
	},

	/** 単純小文字化 */
	LOWER_CASE {

		@Override
		public String convert(final String original) {
			if (original == null || "".equals(original)) {
				return "";
			}
			return original.trim().toLowerCase();
		}
	},

	/** 変換なし */
	NONE {
		@Override
		public String convert(final String original) {
			if (original == null || "".equals(original)) {
				return "";
			}
			return original.trim();
		}

	};

	/**
	 * 書式の変換を行う
	 *
	 * @param original 変換元書式
	 * @return 変換後文字列
	 */
	public abstract String convert(String original);

	/**
	 * 渡された文字列が大文字のみで構成されるかどうかを判定する.
	 *
	 * @param str 判定対象文字列
	 * @return 大文字で構成される場合は<code>true</code>
	 */
	private static boolean isUpperCase(final String str) {
		if (str == null || "".equals(str)) {
			return false;
		} else {
			return str.equals(str.toUpperCase());
		}
	}
}