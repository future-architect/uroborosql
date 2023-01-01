/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.command;

import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;

import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;

import jp.co.future.uroborosql.client.completer.TableNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * Describe table Command
 *
 * @author H.Sugimoto
 */
public class DescCommand extends ReplCommand {
	/** DESCで表示する項目 */
	private static final String[] DESC_COLUMN_LABELS = { "TABLE_NAME", "COLUMN_NAME", "TYPE_NAME", "COLUMN_SIZE",
			"DECIMAL_DIGITS", "IS_NULLABLE", "COLUMN_DEF", "REMARKS" };

	/**
	 * Constructor
	 */
	@SuppressWarnings("unchecked")
	public DescCommand() {
		super(false, TableNameCompleter.class);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.client.command.ReplCommand#execute(org.jline.reader.LineReader, java.lang.String[], jp.co.future.uroborosql.config.SqlConfig, java.util.Properties)
	 */
	@Override
	public boolean execute(final LineReader reader, final String[] parts, final SqlConfig sqlConfig,
			final Properties props) {
		var writer = reader.getTerminal().writer();

		var tableNamePattern = parts.length > 1 ? parts[parts.length - 1] : "%";

		try {
			var conn = sqlConfig.getConnectionSupplier().getConnection();
			var md = conn.getMetaData();

			List<Map<String, String>> columns = new ArrayList<>();
			Map<String, Integer> labelLength = new HashMap<>();
			for (var label : DESC_COLUMN_LABELS) {
				labelLength.put(label, label.length());
			}
			try (var rs = md.getColumns(conn.getCatalog(), conn.getSchema(), tableNamePattern, null)) {
				while (rs.next()) {
					Map<String, String> column = new HashMap<>();
					for (var label : DESC_COLUMN_LABELS) {
						var value = Objects.toString(rs.getString(label), "");
						column.put(label, value);
						labelLength.compute(
								label,
								(k, v) -> v == null ? getByteLength(value)
										: v.compareTo(getByteLength(value)) >= 0 ? v : getByteLength(value));
					}
					columns.add(column);
				}
			}

			// ラベル
			writer.print("-");
			for (var label : DESC_COLUMN_LABELS) {
				writer.print(StringUtils.rightPad("", labelLength.get(label), "-"));
				writer.print("-");
			}
			writer.println();
			writer.print("|");
			for (var label : DESC_COLUMN_LABELS) {
				writer.print(StringUtils.rightPad(label, labelLength.get(label)));
				writer.print("|");
			}
			writer.println();
			// カラムデータ
			String tableName = null;
			var breakFlag = false;
			for (var column : columns) {
				if (tableName == null || !tableName.equalsIgnoreCase(column.get("TABLE_NAME"))) {
					tableName = column.get("TABLE_NAME");
					breakFlag = true;
				}
				if (breakFlag) {
					writer.print("-");
					for (var label : DESC_COLUMN_LABELS) {
						writer.print(StringUtils.rightPad("", labelLength.get(label), "-"));
						writer.print("-");
					}
					writer.println();
					breakFlag = false;
				}

				writer.print("|");
				for (var label : DESC_COLUMN_LABELS) {
					var val = column.get(label);
					if (StringUtils.isNumeric(val)) {
						writer.print(StringUtils.leftPad(val, labelLength.get(label)));
					} else {
						writer.print(StringUtils.rightPad(val, labelLength.get(label)));
					}
					writer.print("|");
				}
				writer.println();
			}
			writer.print("-");
			for (var label : DESC_COLUMN_LABELS) {
				writer.print(StringUtils.rightPad("", labelLength.get(label), "-"));
				writer.print("-");
			}
			writer.println();
		} catch (SQLException ex) {
			ex.printStackTrace();
		}
		writer.flush();

		return true;
	}

	/**
	 * オブジェクトの文字列表現のバイト数（デフォルトエンコーディング）を取得する
	 *
	 * @param val 計算対象オブジェクト
	 * @return バイト数
	 */
	private int getByteLength(final Object val) {
		if (val == null) {
			return 0;
		}
		var str = val.toString();
		try {
			return str.getBytes(Charset.defaultCharset().displayName()).length;
		} catch (UnsupportedEncodingException ex) {
			return 1;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.client.command.ReplCommand#showHelp(org.jline.terminal.Terminal)
	 */
	@Override
	public void showHelp(final Terminal terminal) {
		terminal.writer().println("\t" + this.toString() + "\t: describe table.");
		terminal.writer().println("\t\tex) desc [table name]<Enter> : Show table description.");
	}

}
