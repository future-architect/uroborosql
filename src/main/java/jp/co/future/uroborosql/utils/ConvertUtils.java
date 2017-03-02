package jp.co.future.uroborosql.utils;

/**
 * 変換を行うためのメソッドを提供するユーティリティクラス
 *
 * @author H.Sugimoto
 */
public final class ConvertUtils {
	private ConvertUtils() {
		// do nothing
	}

	/**
	 * キャメル式文字列をスネーク式文字列（大文字）に変換する
	 *
	 * <pre>
	 * 	toSnake("snake")    ⇒	"SNAKE"
	 * 	toSnake("toSnake")  ⇒	"TO_SNAKE"
	 * 	toSnake("")         ⇒	""
	 * 	toSnake(null)       ⇒	""
	 * </pre>
	 *
	 * @param camel キャメル式文字列
	 * @return スネーク式文字列（大文字）
	 */
	public static String toSnake(final String camel) {
		if (camel == null || "".equals(camel)) {
			return "";
		}
		StringBuilder builder = new StringBuilder();
		String str = camel.trim();
		for (int i = 0; i < str.length(); i++) {
			char ch = str.charAt(i);
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
		}
		return builder.toString();
	}

	/**
	 * スネーク式文字列をキャメル式文字列に変換する
	 *
	 * <pre>
	 * 	toCamel("CAMEL")    ⇒	"camel"
	 * 	toCamel("TO_CAMEL") ⇒	"toCamel"
	 * 	toCamel("")         ⇒	""
	 * 	toCamel(null)       ⇒	""
	 * </pre>
	 *
	 * @param snake スネーク式文字列
	 * @return キャメル式文字列
	 */
	public static String toCamel(final String snake) {
		if (snake == null || "".equals(snake)) {
			return "";
		}
		StringBuilder builder = new StringBuilder();
		String str = snake.trim().toLowerCase();
		int i = 0;
		int len = str.length();
		while (i < len) {
			char ch = str.charAt(i);
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

}
