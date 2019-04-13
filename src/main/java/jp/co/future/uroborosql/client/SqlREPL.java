/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.ServiceLoader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.jline.reader.Completer;
import org.jline.reader.EndOfFileException;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.UserInterruptException;
import org.jline.reader.impl.DefaultHighlighter;
import org.jline.reader.impl.completer.AggregateCompleter;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import org.jline.utils.AttributedStringBuilder;
import org.jline.utils.AttributedStyle;
import org.jline.utils.InfoCmp.Capability;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.client.completer.BindParamCompleter;
import jp.co.future.uroborosql.client.completer.ReplCommandCompleter;
import jp.co.future.uroborosql.client.completer.SqlKeywordCompleter;
import jp.co.future.uroborosql.client.completer.SqlNameCompleter;
import jp.co.future.uroborosql.client.completer.TableNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextFactory;
import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;
import jp.co.future.uroborosql.filter.DumpResultSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.mapping.Table;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.store.NioSqlManagerImpl;

/**
 * SQL REPL実装クラス
 *
 * SQLの実行をREPL形式で行うことができる
 *
 * @author H.Sugimoto
 */
public class SqlREPL {
	/** プロパティ上のクラスパスに指定された環境変数を置換するための正規表現 */
	private static final Pattern SYSPROP_PAT = Pattern.compile("\\$\\{(.+?)\\}");

	/** DESCで表示する項目 */
	private static final String[] DESC_COLUMN_LABELS = { "TABLE_NAME", "COLUMN_NAME", "TYPE_NAME", "COLUMN_SIZE",
			"DECIMAL_DIGITS", "IS_NULLABLE", "COLUMN_DEF", "REMARKS" };

	/** プロパティパス */
	private final Path propPath;

	/** プロパティ */
	private Properties props;

	/** 追加で読み込むクラスローダ */
	private URLClassLoader additionalClassLoader = null;

	/** SQL設定クラス */
	SqlConfig config = null;

	/**
	 * メインメソッド
	 *
	 * @param args 読み込むプロパティファイルのファイルパス
	 */
	public static void main(final String... args) {
		((Logger) LoggerFactory.getLogger("jp.co.future.uroborosql")).setLevel(Level.DEBUG);

		String propFile = "repl.properties";
		if (args.length != 0) {
			propFile = args[0];
		}

		Path path = Paths.get(propFile);

		if (!Files.exists(path)) {
			throw new IllegalArgumentException("properties could not found.");
		}

		try {
			SqlREPL repl = new SqlREPL(path);
			repl.execute();
		} catch (Exception ex) {
			throw new IllegalStateException("Failed to REPL.", ex);
		}
	}

	/**
	 * プロパティファイルの読み込み
	 *
	 * @param path プロパティファイルパス
	 * @return プロパティ
	 */
	private Properties loadProps(final Path path) {
		Properties props = new Properties();
		try {
			InputStream is = Files.newInputStream(path);
			props.load(new InputStreamReader(is, Charset.forName("UTF-8")));
		} catch (IOException ex) {
			throw new IllegalArgumentException("Failed to load properties.", ex);
		}
		return props;
	}

	/**
	 * コンストラクタ
	 *
	 * @param path プロパティファイルパス
	 * @throws Exception 実行時例外
	 */
	public SqlREPL(final Path path) throws Exception {
		propPath = path;
		props = loadProps(path);
	}

	/**
	 * REPLの実行
	 *
	 * @throws Exception 実行時例外
	 */
	private void execute() throws Exception {
		try (Terminal terminal = TerminalBuilder.builder().build()) {
			showMessage(terminal, "/message.txt");
			showProps(terminal);

			initialize(terminal);

			listen(terminal);

			dispose(terminal);
		}
	}

	/**
	 * 初期化処理
	 *
	 * @param terminal Terminal
	 * @throws Exception 実行時例外
	 */
	private void initialize(final Terminal terminal) throws Exception {
		terminal.writer().println("initialize.");

		ClassLoader currentClassLoader = Thread.currentThread().getContextClassLoader();

		if (currentClassLoader.equals(additionalClassLoader)) {
			// すでに追加クラスローダになっている場合はいったん親に移動しておく
			currentClassLoader = additionalClassLoader.getParent();
		}

		String paths = p("sql.additionalClassPath", ".");
		List<URL> urls = new ArrayList<>();
		Arrays.stream(paths.split(";")).forEach(path -> {
			try {
				Matcher m = SYSPROP_PAT.matcher(path);
				StringBuffer sb = new StringBuffer();
				while (m.find()) {
					String key = m.group(1);
					String val = System.getProperty(key, null);
					if (val == null) {
						val = System.getenv(key);
					}
					if (val == null) {
						throw new IllegalArgumentException("key=" + key + " is not found.");
					}
					m.appendReplacement(sb, val.replace("\\", "/"));
				}
				m.appendTail(sb);

				urls.add(Paths.get(sb.toString()).toUri().toURL());
			} catch (Exception e) {
				e.printStackTrace();
			}
		});
		additionalClassLoader = new URLClassLoader(urls.toArray(new URL[urls.size()]), currentClassLoader);

		Thread.currentThread().setContextClassLoader(additionalClassLoader);

		ServiceLoader<Driver> loader = ServiceLoader.load(Driver.class, additionalClassLoader);
		loader.forEach(driver -> {
			try {
				DriverManager.registerDriver(new DriverShim(driver));
			} catch (Exception e) {
				e.printStackTrace();
			}
		});

		String url = p("db.url", "");
		String user = p("db.user", "");
		String password = p("db.password", "");
		String schema = p("db.schema", null);
		String loadPath = p("sql.loadPath", "sql");
		String fileExtension = p("sql.fileExtension", ".sql");
		Charset charset = Charset.forName(p("sql.encoding", "UTF-8"));
		boolean detectChanges = BooleanUtils.toBoolean(p("sql.detectChanges", "true"));

		// config
		config = UroboroSQL.builder(url, user, password, schema)
				.setSqlManager(new NioSqlManagerImpl(loadPath, fileExtension, charset, detectChanges))
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new DumpResultSqlFilter())).build();

		// sqlContextFactory
		SqlContextFactory contextFactory = config.getSqlContextFactory();
		List<String> constantClassNames = Arrays
				.asList(p("sqlContextFactory.constantClassNames", "").split("\\s*,\\s*")).stream()
				.filter(s -> StringUtils.isNotEmpty(s)).collect(Collectors.toList());
		if (!constantClassNames.isEmpty()) {
			contextFactory.setConstantClassNames(constantClassNames);
		}

		List<String> enumConstantPackageNames = Arrays
				.asList(p("sqlContextFactory.enumConstantPackageNames", "").split("\\s*,\\s*")).stream()
				.filter(s -> StringUtils.isNotEmpty(s)).collect(Collectors.toList());
		if (!enumConstantPackageNames.isEmpty()) {
			contextFactory.setEnumConstantPackageNames(enumConstantPackageNames);
		}

		if (!constantClassNames.isEmpty() || !enumConstantPackageNames.isEmpty()) {
			contextFactory.initialize();
		}
	}

	/**
	 * プロパティの取得
	 * @param key キー
	 * @param defaultValue 初期値
	 * @return 値
	 */
	private String p(final String key, final String defaultValue) {
		return props.getProperty(key, defaultValue);
	}

	/**
	 * 終了時処理
	 * @param terminal Terminal
	 */
	private void dispose(final Terminal terminal) {
		terminal.writer().println("SQL REPL end.");
		terminal.writer().flush();

		try {
			NioSqlManagerImpl sqlManager = (NioSqlManagerImpl) config.getSqlManager();
			sqlManager.shutdown();
		} catch (Exception ex) {
			// do nothing
		}
	}

	/**
	 * コンソールからの入力のリスン
	 *
	 * @param terminal Terminal
	 * @throws IOException I/O例外
	 */
	private void listen(final Terminal terminal) throws IOException {
		List<Completer> completers = new ArrayList<>();

		// コマンドのコード補完
		completers.add(new ReplCommandCompleter());
		// SQL名のコード補完
		completers.add(new SqlNameCompleter(config.getSqlManager()));
		// バインドパラメータのコード補完
		completers.add(new BindParamCompleter(config.getSqlManager()));
		// テーブル名のコード補完
		completers.add(new TableNameCompleter(config.getConnectionSupplier()));
		// SQLキーワードのコード補完
		completers.add(new SqlKeywordCompleter());

		LineReader reader = LineReaderBuilder.builder()
				.appName("uroborosql")
				.terminal(terminal)
				.completer(new AggregateCompleter(completers))
				.highlighter(new DefaultHighlighter())
				.option(LineReader.Option.CASE_INSENSITIVE, true)
				.build();
		while (true) { // ユーザの一行入力を待つ
			try {
				if (!commandExecute(reader)) {
					break;
				}
			} catch (UserInterruptException | EndOfFileException ex) {
				break;
			} catch (Exception e) {
				e.printStackTrace(System.err);
			}
		}
	}

	/**
	 * 入力から指定されたコマンドを判定し実行する。
	 *
	 * @param reader LineReader
	 * @return 入力を継続する場合は<code>true</code>
	 * @throws Exception 実行時例外
	 */
	private boolean commandExecute(final LineReader reader) throws Exception {
		String prompt = new AttributedStringBuilder()
				.style(AttributedStyle.BOLD.foreground(AttributedStyle.GREEN))
				.ansiAppend("uroborosql")
				.style(AttributedStyle.DEFAULT)
				.append(" > ")
				.toAnsi();
		String line = reader.readLine(prompt);
		if (line == null || line.isEmpty()) {
			// 空なら何もせずにループ
			return true;
		}

		String[] parts = line.split("\\s+");
		ReplCommand command = ReplCommand.toCommand(parts[0]);

		PrintWriter writer = reader.getTerminal().writer();

		switch (command) {
		case EXIT:
			return false;

		case RELOAD:
			writer.println("RELOAD " + propPath);
			writer.flush();

			for (Enumeration<Driver> drivers = DriverManager.getDrivers(); drivers.hasMoreElements();) {
				Driver driver = drivers.nextElement();
				if (driver instanceof DriverShim) {
					DriverManager.deregisterDriver(driver);
				}
			}
			props = loadProps(propPath);
			showProps(reader.getTerminal());
			initialize(reader.getTerminal());

			return true;

		case LIST:
			writer.println("LIST:");
			writer.flush();

			List<String> pathList = null;
			if (parts.length > 1) {
				pathList = config.getSqlManager().getSqlPathList().stream().filter(p -> p.contains(parts[1]))
						.collect(Collectors.toList());
			} else {
				pathList = config.getSqlManager().getSqlPathList();
			}
			for (String key : pathList) {
				writer.println(key);
			}
			writer.flush();
			return true;

		case HISTORY:
			writer.println("HISTORY:");
			writer.flush();

			List<String> keywords = new ArrayList<>();
			if (parts.length > 1) {
				keywords.addAll(Arrays.asList(Arrays.copyOfRange(parts, 1, parts.length)));
			}

			int sizeLen = String.valueOf(reader.getHistory().size()).length();
			reader.getHistory().forEach(entry -> {
				try {
					String value = entry.line();
					if (keywords.isEmpty() || keywords.stream().anyMatch(s -> value.contains(s))) {
						writer.println(String.format("%" + sizeLen + "d : %s", entry.index() + 1, value));
					}
				} catch (Exception e) {
					// do nothing
				}
			});
			writer.flush();
			return true;

		case DRIVER:
			writer.println("DRIVER:");
			writer.flush();

			Enumeration<Driver> drivers = DriverManager.getDrivers();
			int driverCount = 0;
			while (drivers.hasMoreElements()) {
				Driver driver = drivers.nextElement();
				writer.println(String.format("%02d : %s%n", ++driverCount, driver));
			}

			writer.flush();
			return true;

		case DESC:
			writer.println("DESC:");
			writer.flush();

			String tableNamePattern = parts.length > 1 ? parts[parts.length - 1] : "%";

			try {
				Connection conn = config.getConnectionSupplier().getConnection();
				DatabaseMetaData md = conn.getMetaData();

				List<Map<String, String>> columns = new ArrayList<>();
				Map<String, Integer> labelLength = new HashMap<>();
				for (String label : DESC_COLUMN_LABELS) {
					labelLength.put(label, label.length());
				}
				try (ResultSet rs = md.getColumns(conn.getCatalog(), conn.getSchema(), tableNamePattern, null)) {
					while (rs.next()) {
						Map<String, String> column = new HashMap<>();
						for (String label : DESC_COLUMN_LABELS) {
							final String value = StringUtils.defaultString(rs.getString(label));
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
				for (String label : DESC_COLUMN_LABELS) {
					writer.print(StringUtils.rightPad("", labelLength.get(label), "-"));
					writer.print("-");
				}
				writer.println();
				writer.print("|");
				for (String label : DESC_COLUMN_LABELS) {
					writer.print(StringUtils.rightPad(label, labelLength.get(label)));
					writer.print("|");
				}
				writer.println();
				// カラムデータ
				String tableName = null;
				boolean breakFlag = false;
				for (Map<String, String> column : columns) {
					if (tableName == null || !tableName.equalsIgnoreCase(column.get("TABLE_NAME"))) {
						tableName = column.get("TABLE_NAME");
						breakFlag = true;
					}
					if (breakFlag) {
						writer.print("-");
						for (String label : DESC_COLUMN_LABELS) {
							writer.print(StringUtils.rightPad("", labelLength.get(label), "-"));
							writer.print("-");
						}
						writer.println();
						breakFlag = false;
					}

					writer.print("|");
					for (String label : DESC_COLUMN_LABELS) {
						String val = column.get(label);
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
				for (String label : DESC_COLUMN_LABELS) {
					writer.print(StringUtils.rightPad("", labelLength.get(label), "-"));
					writer.print("-");
				}
				writer.println();
			} catch (SQLException ex) {
				ex.printStackTrace();
			}
			writer.flush();

			return true;

		case GENERATE:
			writer.println("GENERATE:");
			writer.flush();

			if (parts.length < 3) {
				writer.println(
						ReplCommand.GENERATE.toString() + " parameter missing. " + ReplCommand.GENERATE.toString()
								+ " [SQL_KEYWORD] [TABLE NAME].");
				return true;
			}

			String sqlKeyword = parts[1];
			String tableName = parts[2];

			try (SqlAgent agent = config.agent()) {
				Table table = new Table() {

					@Override
					public String getSchema() {
						return null;
					}

					@Override
					public String getName() {
						return tableName;
					}
				};
				TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent, table);
				metadata.setSchema(null);

				SqlContext ctx = null;
				switch (sqlKeyword) {
				case "insert":
					ctx = config.getEntityHandler().createInsertContext(agent, metadata, null);
					break;

				case "update":
					ctx = config.getEntityHandler().createUpdateContext(agent, metadata, null, true);
					break;

				case "delete":
					ctx = config.getEntityHandler().createDeleteContext(agent, metadata, null, true);
					break;

				default:
					ctx = config.getEntityHandler().createSelectContext(agent, metadata, null, true);
					break;
				}
				writer.println(ctx.getSql());
			}

			writer.flush();

			return true;

		case HELP:
			writer.println("HELP:");
			writer.flush();
			showMessage(reader.getTerminal(), "/message.txt");

			return true;

		case THIS:
			showMessage(reader.getTerminal(), "/this.txt");

			return true;

		case CLS:
			reader.getTerminal().puts(Capability.clear_screen);
			reader.getTerminal().flush();
			return true;

		case VIEW:
			if (parts.length == 2) {
				String sqlName = parts[1].replaceAll("\\.", "/");
				if (config.getSqlManager().existSql(sqlName)) {
					String sql = config.getSqlManager().getSql(sqlName);
					String[] sqlLines = sql.split("\\r\\n|\\r|\\n");
					for (String sqlLine : sqlLines) {
						writer.println(sqlLine);
					}
				} else {
					writer.println("SQL not found. sql=" + sqlName);
				}
			}
			writer.flush();
			return true;

		case QUERY:
			if (parts.length >= 2) {
				String sqlName = parts[1].replaceAll("\\.", "/");
				if (config.getSqlManager().existSql(sqlName)) {
					try (SqlAgent agent = config.agent()) {
						SqlContext ctx = agent.contextFrom(sqlName);
						ctx.setSql(config.getSqlManager().getSql(ctx.getSqlName()));
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

		case UPDATE:
			if (parts.length >= 2) {
				String sqlName = parts[1].replaceAll("\\.", "/");
				if (config.getSqlManager().existSql(sqlName)) {

					try (SqlAgent agent = config.agent()) {
						SqlContext ctx = agent.contextFrom(sqlName);
						ctx.setSql(config.getSqlManager().getSql(ctx.getSqlName()));
						String[] params = Arrays.copyOfRange(parts, 2, parts.length);
						SqlParamUtils.setSqlParams(ctx, params);
						try {
							int ans = agent.update(ctx);
							agent.commit();
							writer.println("update sql[" + sqlName + "] end. row count=" + ans);
						} catch (ParameterNotFoundRuntimeException | SQLException ex) {
							writer.println("Error : " + ex.getMessage());
							agent.rollback();
						}
					}
				} else {
					writer.println("SQL not found. sql=" + sqlName);
				}
			}
			writer.flush();
			return true;

		default:
			return true;
		}
	}

	/**
	 * メッセージの表示
	 * @throws IOException IO例外
	 */
	private void showMessage(final Terminal terminal, final String path) throws IOException {
		String messageFilePath = this.getClass().getPackage().getName().replace(".", "/") + path;
		try (BufferedReader reader = new BufferedReader(new InputStreamReader(Thread.currentThread()
				.getContextClassLoader().getResourceAsStream(messageFilePath), Charset.forName("UTF-8")))) {
			reader.lines().forEach(s -> {
				try {
					terminal.writer().println(s);
				} catch (Exception ex) {
					// ここで例外が出てもメッセージ表示が正しく出ないだけなので、エラーを握りつぶす
				}
			});
		}
		terminal.flush();
	}

	/**
	 * 現在読み込んでいるプロパティの情報を表示
	 * @throws IOException IO例外
	 */
	private void showProps(final Terminal terminal) throws IOException {
		terminal.writer().println("[Properties]");
		props.forEach((key, value) -> {
			try {
				terminal.writer().println(key + "=" + value);
			} catch (Exception e) {
				// ここで例外が出てもメッセージ表示が正しく出ないだけなので、エラーを握りつぶす
			}
		});
		terminal.writer().println();
		terminal.flush();
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
		String str = val.toString();
		try {
			return str.getBytes(System.getProperty("file.encoding")).length;
		} catch (UnsupportedEncodingException ex) {
			return 1;
		}
	}

}
