/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.command;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.jline.reader.Completer;
import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;

import jp.co.future.uroborosql.client.SqlREPL;
import jp.co.future.uroborosql.config.SqlConfig;

/**
 * REPLで実行するコマンドの抽象親クラス
 *
 * @author H.Sugimoto
 */
public abstract class ReplCommand {
	/** 利用する入力補完の並び */
	private final List<Class<? extends Completer>> completers;
	/** HELPコマンドで非表示 */
	private final boolean hidden;

	/**
	 * 入力コマンド
	 *
	 * @param hidden HELPコマンドで表示しない場合に<code>true</code>
	 * @param completers 補完器名
	 */
	@SuppressWarnings("unchecked")
	protected ReplCommand(final boolean hidden, final Class<? extends Completer>... completers) {
		this.hidden = hidden;
		this.completers = Arrays.asList(completers);
	}

	/**
	 * 指定されたコード補完の対象とする引数の開始位置を返却する
	 *
	 * @param completer 対象とするコード補完名
	 * @return 引数の位置。該当するコード補完がない場合は<code>-1</code>を返す
	 */
	public int getStartArgNo(final Class<? extends Completer> completer) {
		int idx = this.completers.indexOf(completer);
		return idx >= 0 ? idx + 1 : -1;
	}

	/**
	 * 非表示コマンドかどうか
	 * @return 非表示コマンドの場合<code>true</code>
	 */
	public boolean isHidden() {
		return hidden;
	}

	/**
	 * 指定された入力がコマンドにマッチするかどうかを判定する
	 * @param input 入力文字列
	 * @return コマンドにマッチする場合<code>true</code>
	 */
	public boolean match(final String input) {
		return !isHidden() && toString().startsWith(input.toLowerCase());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.lang.Enum#toString()
	 */
	@Override
	public String toString() {
		return this.getClass().getSimpleName().replace("Command", "").toLowerCase();
	}

	/**
	 * 引数で指定したコマンド名にこのコマンドが一致するかどうか
	 * @param commandName コマンド名文字列
	 * @return コマンドが一致する場合<code>true</code>
	 */
	public boolean is(final String commandName) {
		return toString().equalsIgnoreCase(commandName);
	}

	/**
	 * コマンドの実行
	 *
	 * @param reader LineReader
	 * @param parts Command line parts
	 * @param sqlConfig SqlConfig
	 * @param props properties
	 * @return REPLを継続する場合<code>true</code>
	 */
	public abstract boolean execute(final LineReader reader, final String[] parts, final SqlConfig sqlConfig,
			final Properties props);

	/**
	 * コマンドのヘルプメッセージをターミナルに出力する
	 *
	 * @param terminal Terminal
	 */
	public abstract void showHelp(Terminal terminal);

	/**
	 * メッセージの表示
	 *
	 * @param terminal Terminal
	 * @param path show file path
	 */
	protected void showMessage(final Terminal terminal, final String path) {
		String messageFilePath = SqlREPL.class.getPackage().getName().replace(".", "/") + path;
		try (BufferedReader reader = new BufferedReader(new InputStreamReader(Thread.currentThread()
				.getContextClassLoader().getResourceAsStream(messageFilePath), Charset.forName("UTF-8")))) {
			reader.lines().forEach(s -> {
				try {
					terminal.writer().println(s);
				} catch (Exception ex) {
					// ここで例外が出てもメッセージ表示が正しく出ないだけなので、エラーを握りつぶす
				}
			});
		} catch (IOException e) {
			// ここで例外が出てもメッセージ表示が正しく出ないだけなので、エラーを握りつぶす
		}
		terminal.flush();
	}

}
