/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.command;

import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Properties;

import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.client.SqlParamUtils;
import jp.co.future.uroborosql.client.completer.BindParamCompleter;
import jp.co.future.uroborosql.client.completer.SqlNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;

/**
 * Execute sql query Command
 *
 * @author H.Sugimoto
 */
public class QueryCommand extends ReplCommand {

	/**
	 * Constructor
	 */
	@SuppressWarnings("unchecked")
	public QueryCommand() {
		super(false, SqlNameCompleter.class, BindParamCompleter.class);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.client.command.ReplCommand#execute(org.jline.reader.LineReader, java.lang.String[], jp.co.future.uroborosql.config.SqlConfig, java.util.Properties)
	 */
	@Override
	public boolean execute(final LineReader reader, final String[] parts, final SqlConfig sqlConfig,
			final Properties props) {
		PrintWriter writer = reader.getTerminal().writer();
		if (parts.length >= 2) {
			String sqlName = parts[1].replaceAll("\\.", "/");
			if (sqlConfig.getSqlManager().existSql(sqlName)) {
				try (SqlAgent agent = sqlConfig.agent()) {
					SqlContext ctx = agent.contextFrom(sqlName);
					ctx.setSql(sqlConfig.getSqlManager().getSql(ctx.getSqlName()));
					String[] params = Arrays.copyOfRange(parts, 2, parts.length);
					SqlParamUtils.setSqlParams(ctx, params);

					ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

					try (ResultSet rs = agent.query(ctx)) {
						writer.println("query sql[" + sqlName + "] end.");
					} catch (ParameterNotFoundRuntimeException | SQLException ex) {
						writer.println("Error : " + ex.getMessage());
					} finally {
						agent.rollback();
					}
				}
			} else {
				writer.println("SQL not found. sql=" + sqlName);
			}
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
		terminal.writer().println("\t" + this.toString() + "\t: execute query from loaded sql file.");
		terminal.writer().println("\t\tex1) query [sql file name]<Enter> : Execute SQL without parameter.");
		terminal.writer().println(
				"\t\tex2) query [sql file name] param1=val1 param2=val2 ...<Enter> : Execute SQL with the specified parameters.");
	}
}
