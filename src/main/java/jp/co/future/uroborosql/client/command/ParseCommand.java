/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client.command;

import java.io.PrintWriter;
import java.util.LinkedHashSet;
import java.util.Properties;
import java.util.Set;

import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;

import jp.co.future.uroborosql.client.completer.SqlNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.expr.ExpressionParser;
import jp.co.future.uroborosql.node.BeginNode;
import jp.co.future.uroborosql.node.ContainerNode;
import jp.co.future.uroborosql.node.ElseNode;
import jp.co.future.uroborosql.node.ExpressionNode;
import jp.co.future.uroborosql.node.IfNode;
import jp.co.future.uroborosql.node.Node;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * Parse sql Command
 *
 * @author H.Sugimoto
 */
public class ParseCommand extends ReplCommand {
	/**
	 * Constructor
	 */
	@SuppressWarnings("unchecked")
	public ParseCommand() {
		super(false, SqlNameCompleter.class);
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

		writer.println("PARSE:");

		if (parts.length > 1) {
			var path = sqlConfig.getSqlManager().getSqlPathList().stream()
					.filter(p -> p.equalsIgnoreCase(parts[1]))
					.findFirst();
			if (path.isPresent()) {
				var sql = sqlConfig.getSqlManager().getSql(path.get());

				// 対象SQLの出力
				writer.println("");
				writer.println("SQL :");
				var sqlLines = sql.split("\\r\\n|\\r|\\n");
				for (String sqlLine : sqlLines) {
					writer.println(sqlLine);
				}

				// IF分岐の出力
				writer.println("");
				writer.println("BRANCHES :");
				SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
						sqlConfig.getDialect().isRemoveTerminator(), true);
				var transformer = parser.parse();
				var rootNode = transformer.getRoot();
				Set<String> bindParams = new LinkedHashSet<>();
				traverseNode(writer, 0, sqlConfig.getExpressionParser(), rootNode, bindParams);

				// 利用されているバインドパラメータの出力
				writer.println("");
				var constPrefix = sqlConfig.getSqlContextFactory().getConstParamPrefix();
				var ctx = sqlConfig.getSqlContextFactory().createSqlContext();
				writer.println("BIND_PARAMS :");
				// 定数以外のバインドパラメータ
				bindParams.stream().filter(param -> !param.startsWith(constPrefix))
						.sorted()
						.forEach(param -> write(writer, 1, null, param, null));
				// 定数バインドパラメータ
				bindParams.stream().filter(param -> param.startsWith(constPrefix))
						.sorted().forEach(param -> {
							// 定数についてはバインド値が取得できるので、実際の値を追加で表示
							var parameter = ctx.getParam(param);
							if (parameter != null) {
								var value = parameter.getValue();
								String suffix = null;
								if (value instanceof String) {
									suffix = String.format(" ('%s')", value);
								} else {
									suffix = String.format(" (%s)", value);
								}
								write(writer, 1, null, param, suffix);
							} else {
								write(writer, 1, null, param, " (not found)");
							}
						});
			} else {
				writer.println("sqlName : " + parts[1] + " not found.");
			}
		} else {
			writer.println("sqlName must be specified.");
		}
		writer.flush();
		return true;

	}

	/**
	 * SqlNodeを探索する.
	 *
	 * @param writer Writer
	 * @param tab 階層
	 * @param expressionParser ExpressionParser
	 * @param node 探索するNode
	 * @param bindParams 発見したBindParamを格納するSet
	 */
	private void traverseNode(final PrintWriter writer, final int tab, final ExpressionParser expressionParser,
			final Node node, final Set<String> bindParams) {
		if (node instanceof ContainerNode) {
			if (node instanceof IfNode) {
				traverseIfNode(writer, tab, expressionParser, (IfNode) node, bindParams, false);
			} else {
				if (node instanceof BeginNode) {
					write(writer, tab, null, "BEGIN", " {");
				} else if (node instanceof ElseNode) {
					write(writer, tab, "} ", "ELSE", " {");
				}
				for (var i = 0; i < node.getChildSize(); i++) {
					traverseNode(writer, tab + 1, expressionParser, node.getChild(i), bindParams);
				}
				if (node instanceof BeginNode) {
					write(writer, tab, null, "}", null);
				}
			}
		} else if (node instanceof ExpressionNode) {
			var expression = ((ExpressionNode) node).getExpression();
			bindParams.addAll(collectParams(expressionParser, expression));
		}
	}

	/**
	 * IFNodeの探索.
	 *
	 * @param writer Writer
	 * @param tab 階層
	 * @param expressionParser ExpressionParser
	 * @param ifNode 探索するIfNode
	 * @param bindParams 発見したBindParamを格納するSet
	 * @param elseIf ElseIfかどうか
	 */
	private void traverseIfNode(final PrintWriter writer, final int tab, final ExpressionParser expressionParser,
			final IfNode ifNode, final Set<String> bindParams, final boolean elseIf) {
		bindParams.addAll(collectParams(expressionParser, ifNode.getExpression()));
		write(writer, tab, elseIf ? "} ELIF ( " : "IF ( ", ifNode.getExpression(), " ) {");
		for (var i = 0; i < ifNode.getChildSize(); i++) {
			traverseNode(writer, tab + 1, expressionParser, ifNode.getChild(i), bindParams);
		}

		if (ifNode.getElseIfNode() != null) {
			var elseIfNode = ifNode.getElseIfNode();
			traverseIfNode(writer, tab, expressionParser, elseIfNode, bindParams, true);
		}

		if (ifNode.getElseNode() != null) {
			traverseNode(writer, tab, expressionParser, ifNode.getElseNode(), bindParams);
		}
		if (!elseIf) {
			write(writer, tab, null, "}", null);
		}
	}

	/**
	 * 評価式に含まれるバインドパラメータの取得
	 * @param expressionParser ExpressionParser
	 * @param expression 評価式
	 * @return 評価式に含まれるバインドパラメータの集合
	 */
	private Set<String> collectParams(final ExpressionParser expressionParser, final String expression) {
		Set<String> params = new LinkedHashSet<>();
		expressionParser.parse(expression).collectParams(params);
		return params;

	}

	/**
	 * テキストの出力.
	 *
	 * @param writer Writer
	 * @param tab 階層
	 * @param prefix プレフィックス
	 * @param expression 評価式
	 * @param suffix サフィックス
	 */
	private void write(final PrintWriter writer, final int tab, final String prefix, final String text,
			final String suffix) {
		writer.write(StringUtils.repeat('\t', tab));
		if (StringUtils.isNotEmpty(prefix)) {
			writer.write(prefix);
		}
		writer.write(text);
		if (StringUtils.isNotEmpty(suffix)) {
			writer.write(suffix);
		}
		writer.println();
	}

	@Override
	public void showHelp(final Terminal terminal) {
		terminal.writer().println("\t" + this.toString() + "\t: parse sql file.");
		terminal.writer().println("\t\tex) parse [sql file name]<Enter> : Parse sql file.");
	}

}
