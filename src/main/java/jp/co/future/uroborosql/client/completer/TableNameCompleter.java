/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.completer;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import org.jline.reader.Candidate;
import org.jline.reader.LineReader;
import org.jline.reader.ParsedLine;

import jp.co.future.uroborosql.client.command.ReplCommand;
import jp.co.future.uroborosql.connection.ConnectionSupplier;

/**
 * テーブル名を補完するCompleter
 *
 * @author H.Sugimoto
 *
 */
public class TableNameCompleter extends AbstractCompleter {
	private final ConnectionSupplier connectionSupplier;

	/**
	 * Constructor
	 *
	 * @param commands ReplCommand List
	 * @param connectionSupplier connectionSupplier
	 */
	public TableNameCompleter(final List<ReplCommand> commands, final ConnectionSupplier connectionSupplier) {
		super(commands);
		this.connectionSupplier = connectionSupplier;
	}

	/**
	 * DatabaseMetadateからテーブル名を取得して補完候補とする。
	 *
	 * {@inheritDoc}
	 *
	 * @see org.jline.reader.Completer#complete(org.jline.reader.LineReader, org.jline.reader.ParsedLine, java.util.List)
	 */
	@Override
	public void complete(final LineReader reader, final ParsedLine line, final List<Candidate> candidates) {
		String buffer = line.line();
		String[] parts = getLineParts(buffer);
		int len = parts.length;

		// コード補完する引数の番号を特定。
		int startArgNo = getStartArgNo(line);

		// 対象引数が-1、または開始引数にlenが満たない場合は該当なしなのでコード補完しない
		if (!accept(startArgNo, buffer, len)) {
			return;
		}

		boolean isBlank = buffer.endsWith(" ");
		String tableNamePattern = "%";
		if (len == startArgNo && isBlank) {
			tableNamePattern = "%";
		} else if (len == startArgNo + 1 && !isBlank) {
			tableNamePattern = parts[len - 1] + "%";
		} else {
			return;
		}

		try {
			Connection conn = connectionSupplier.getConnection();
			DatabaseMetaData md = conn.getMetaData();
			try (ResultSet rs = md.getTables(conn.getCatalog(), conn.getSchema(),
					tableNamePattern.toUpperCase(), null)) {
				while (rs.next()) {
					candidates.add(new Candidate(rs.getString("TABLE_NAME")));
				}
			}
			if (candidates.isEmpty()) {
				try (ResultSet rs = md.getTables(conn.getCatalog(), conn.getSchema(),
						tableNamePattern.toLowerCase(), null)) {
					while (rs.next()) {
						candidates.add(new Candidate(rs.getString("TABLE_NAME")));
					}
				}
			}
		} catch (SQLException ex) {
			ex.printStackTrace();
			return;
		}
	}
}