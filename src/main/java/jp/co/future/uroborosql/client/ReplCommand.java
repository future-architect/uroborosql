/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.jline.reader.Completer;

import jp.co.future.uroborosql.client.completer.BindParamCompleter;
import jp.co.future.uroborosql.client.completer.SqlKeywordCompleter;
import jp.co.future.uroborosql.client.completer.SqlNameCompleter;
import jp.co.future.uroborosql.client.completer.TableNameCompleter;

/** コマンド名 */
@SuppressWarnings("unchecked")
public enum ReplCommand {
	/** SQLの検索実行 */
	QUERY(false, SqlNameCompleter.class, BindParamCompleter.class),
	/** SQLの更新実行 */
	UPDATE(false, SqlNameCompleter.class, BindParamCompleter.class),
	/** SQLファイルの参照 */
	VIEW(false, SqlNameCompleter.class),
	/** SQLファイルのリスト出力 */
	LIST(false, SqlNameCompleter.class),
	/** 入力コマンドの履歴出力 */
	HISTORY(false),
	/** リロード */
	RELOAD(false),
	/** 登録されているJDBCドライバーのリスト出力 */
	DRIVER(false),
	/** テーブル定義表示 */
	DESC(false, TableNameCompleter.class),
	/** SQL文生成 */
	GENERATE(false, SqlKeywordCompleter.class, TableNameCompleter.class),
	/** ヘルプメッセージ出力 */
	HELP(false),
	/** 画面のクリア */
	CLS(false),
	/** SqlREPLの終了 */
	EXIT(false),
	/** スペシャルメッセージ */
	THIS(true);

	/** 利用する入力補完の並び */
	private List<Class<? extends Completer>> completers = new ArrayList<>();
	/** HELPコマンドで非表示 */
	private boolean hidden = false;

	/**
	 * 入力コマンド
	 *
	 * @param hidden HELPコマンドで表示しない場合に<code>true</code>
	 * @param completers 補完器名
	 */
	private ReplCommand(final boolean hidden, final Class<? extends Completer>... completers) {
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
		int idx = completers.indexOf(completer);
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
		return super.toString().toLowerCase();
	}

	/**
	 * 文字列からCommandを取得する
	 * @param cmd コマンド文字列
	 * @return 対応するCommandクラス。該当するCommandがない場合、HelpCommandを返却する
	 */
	public static ReplCommand toCommand(final String cmd) {
		if (StringUtils.isNotBlank(cmd)) {
			for (ReplCommand command : values()) {
				if (command.toString().equalsIgnoreCase(cmd.trim())) {
					return command;
				}
			}
		}
		return HELP;
	}
}