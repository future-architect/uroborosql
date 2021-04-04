/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.command;

import java.sql.SQLException;
import java.util.Properties;

import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;

import jp.co.future.uroborosql.client.completer.SqlKeywordCompleter;
import jp.co.future.uroborosql.client.completer.SqlKeywordCompleter.SqlKeyword;
import jp.co.future.uroborosql.client.completer.TableNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.mapping.MetaTable;
import jp.co.future.uroborosql.mapping.Table;
import jp.co.future.uroborosql.mapping.TableMetadata;

/**
 * Generate sql template Command
 *
 * @author H.Sugimoto
 */
public class GenerateCommand extends ReplCommand {

	/**
	 * Constructor
	 */
	@SuppressWarnings("unchecked")
	public GenerateCommand() {
		super(false, SqlKeywordCompleter.class, TableNameCompleter.class);
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

		if (parts.length < 3) {
			writer.println(toString() + " parameter missing. " + toString() + " [SQL_KEYWORD] [TABLE NAME].");
			return true;
		}

		try (var agent = sqlConfig.agent()) {
			var sqlKeyword = SqlKeyword.of(parts[1]);
			final var tableName = parts[2];
			final var versionColumnName = props.getProperty("sql.versionColumnName");
			final var optimisticLockSupplier = props
					.getProperty("sql.optimisticLockSupplier",
							"jp.co.future.uroborosql.mapping.LockVersionOptimisticLockSupplier");

			Table table = new MetaTable(tableName, null, versionColumnName, optimisticLockSupplier);
			var metadata = TableMetadata.createTableEntityMetadata(agent, table);
			metadata.setSchema(null);

			ExecutionContext ctx = null;
			if (sqlKeyword.isPresent()) {
				var keyword = sqlKeyword.get();
				switch (keyword) {
				case INSERT:
					ctx = sqlConfig.getEntityHandler().createInsertContext(agent, metadata, null);
					break;

				case UPDATE:
					ctx = sqlConfig.getEntityHandler().createUpdateContext(agent, metadata, null, true);
					break;

				case DELETE:
					ctx = sqlConfig.getEntityHandler().createDeleteContext(agent, metadata, null, true);
					break;

				default:
					ctx = sqlConfig.getEntityHandler().createSelectContext(agent, metadata, null, true);
					break;
				}
			} else {
				ctx = sqlConfig.getEntityHandler().createSelectContext(agent, metadata, null, true);
			}
			writer.println(ctx.getSql());
		} catch (SQLException e) {
			e.printStackTrace();
		}

		writer.flush();

		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.client.command.ReplCommand#showHelp(org.jline.terminal.Terminal)
	 */
	@Override
	public void showHelp(final Terminal terminal) {
		terminal.writer().println("\t" + this.toString() + ": generate sql to access the table.");
		terminal.writer().println(
				"\t\tex) generate [select/insert/update/delete] [table name]<Enter> : Show sql to access tables according to keywords.");
	}
}
