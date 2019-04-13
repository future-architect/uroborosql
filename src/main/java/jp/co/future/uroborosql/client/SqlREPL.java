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
import java.sql.DriverPropertyInfo;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.Time;
import java.sql.Timestamp;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.ServiceLoader;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.jline.reader.Candidate;
import org.jline.reader.Completer;
import org.jline.reader.EndOfFileException;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.ParsedLine;
import org.jline.reader.UserInterruptException;
import org.jline.reader.impl.completer.AggregateCompleter;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import org.jline.utils.InfoCmp.Capability;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextFactory;
import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;
import jp.co.future.uroborosql.filter.DumpResultSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.mapping.Table;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.node.BindVariableNode;
import jp.co.future.uroborosql.node.EmbeddedValueNode;
import jp.co.future.uroborosql.node.IfNode;
import jp.co.future.uroborosql.node.Node;
import jp.co.future.uroborosql.node.ParenBindVariableNode;
import jp.co.future.uroborosql.parser.ContextTransformer;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;
import jp.co.future.uroborosql.store.NioSqlManagerImpl;
import ognl.ASTProperty;
import ognl.Ognl;
import ognl.OgnlException;

/**
 * SQL REPL実装クラス
 *
 * SQLの実行をREPL形式で行うことができる
 *
 * @author H.Sugimoto
 */
public class SqlREPL {
	/** バインドパラメータ中の定数指定を判定するための正規表現 */
	private static final Pattern CONSTANT_PAT = Pattern.compile("^[A-Z][A-Z0-9_-]*$");

	/** プロパティ上のクラスパスに指定された環境変数を置換するための正規表現 */
	private static final Pattern SYSPROP_PAT = Pattern.compile("\\$\\{(.+?)\\}");

	/** DESCで表示する項目 */
	private static final String[] DESC_COLUMN_LABELS = { "TABLE_NAME", "COLUMN_NAME", "TYPE_NAME", "COLUMN_SIZE",
			"DECIMAL_DIGITS", "IS_NULLABLE", "COLUMN_DEF", "REMARKS" };

	/** コマンド名 */
	@SuppressWarnings("unchecked")
	private enum Command {
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
		private Command(final boolean hidden, final Class<? extends Completer>... completers) {
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
		public static Command toCommand(final String cmd) {
			if (StringUtils.isNotBlank(cmd)) {
				for (Command command : values()) {
					if (command.toString().equalsIgnoreCase(cmd.trim())) {
						return command;
					}
				}
			}
			return HELP;
		}

		public static Completer getCommandCompleter() {
			return new Completer() {
				@Override
				public void complete(final LineReader reader, final ParsedLine line, final List<Candidate> candidates) {
					if (line.wordIndex() == 0) {
						Stream.of(Command.values()).filter(c -> c.match(line.word()))
								.map(c -> new Candidate(c.toString()))
								.forEach(candidates::add);
					}
				}
			};
		}
	}

	/** プロパティパス */
	private final Path propPath;

	/** プロパティ */
	private Properties props;

	/** 追加で読み込むクラスローダ */
	private URLClassLoader additionalClassLoader = null;

	/** SQL設定クラス */
	private SqlConfig config = null;

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

			initialize();

			listen(terminal);

			dispose(terminal);
		}
	}

	/**
	 * 初期化処理
	 *
	 * @throws Exception 実行時例外
	 */
	private void initialize() throws Exception {
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
	 * @param terminal ターミナル
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
	 * @param terminal ターミナル
	 * @throws IOException I/O例外
	 */
	private void listen(final Terminal terminal) throws IOException {
		List<Completer> completers = new ArrayList<>();

		// コマンドのコード補完
		completers.add(Command.getCommandCompleter());
		// SQL名のコード補完
		completers.add(new SqlNameCompleter(config.getSqlManager().getSqlPathList()));
		// バインドパラメータのコード補完
		completers.add(new BindParamCompleter());
		// テーブル名のコード補完
		completers.add(new TableNameCompleter());
		// SQLキーワードのコード補完
		completers.add(new SqlKeywordCompleter());

		LineReader reader = LineReaderBuilder.builder()
				.appName("uroborosql")
				.terminal(terminal)
				.completer(new AggregateCompleter(completers))
				.option(LineReader.Option.CASE_INSENSITIVE, true)
				.build();
		while (true) { // ユーザの一行入力を待つ
			try {
				if (!commandExecute(reader)) {
					break;
				}
			} catch (UserInterruptException ex) {
				// do nothing
			} catch (EndOfFileException ex) {
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
		String prompt = "uroborosql > ";
		String line = reader.readLine(prompt);
		if (line == null || line.isEmpty()) {
			// 空なら何もせずにループ
			return true;
		}

		String[] parts = getLineParts(line);
		Command command = Command.toCommand(parts[0]);

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
			initialize();

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
				writer.println(Command.GENERATE.toString() + " parameter missing. " + Command.GENERATE.toString()
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
						setSqlParams(ctx, params);

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
						setSqlParams(ctx, params);
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
	 * 入力行をパーツに分解する。空白で分解した後に条件によって結合を行う。<br>
	 *
	 * 【結合条件】<br>
	 * ・''で囲まれる値には空白を含めることができる。ex) 'String value includes whitespace.'<br>
	 * ・[]で囲まれる値は配列を表す。 ex) [10, 20, 30, 40], ['aaa' , 'bbb' , 'ccc']
	 *
	 * @param line 入力行
	 * @return パーツに分解した文字列配列
	 */
	private String[] getLineParts(final String line) {
		String[] parts = line.split(" ");
		List<String> ans = new ArrayList<>();
		int idx = 0;
		int len = parts.length;
		while (idx < len) {
			if (parts[idx].contains("='") && !parts[idx].endsWith("'")) {
				StringBuilder builder = new StringBuilder(parts[idx++]);
				while (idx < len) {
					builder.append(" ").append(parts[idx]);
					if (parts[idx++].endsWith("'")) {
						break;
					}
				}
				ans.add(builder.toString());
			} else if (parts[idx].contains("=[") && !parts[idx].endsWith("]")) {
				StringBuilder builder = new StringBuilder(parts[idx++]);
				while (idx < len) {
					builder.append(" ").append(parts[idx]);
					if (parts[idx++].endsWith("]")) {
						break;
					}
				}
				ans.add(builder.toString());
			} else {
				String part = parts[idx++];
				if (StringUtils.isNotEmpty(part)) {
					ans.add(part);
				}
			}
		}
		return ans.toArray(new String[ans.size()]);
	}

	/**
	 * SQLバインドパラメータを設定する
	 *
	 * @param ctx SQLコンテキスト
	 * @param paramsArray パラメータ配列
	 */
	private void setSqlParams(final SqlContext ctx, final String[] paramsArray) {
		Set<String> bindParams = getSqlParams(ctx.getSql());

		for (String element : paramsArray) {
			String[] param = element.split("=");
			String key = param[0];
			if (bindParams.remove(key)) {
				// キーがバインドパラメータに存在するときは値を設定する
				if (param.length == 1) {
					// キーだけの指定は値をnullと扱う
					ctx.param(key, null);
				} else {
					String val = param[1];
					setParam(ctx, key, val);
				}
			}
		}

		// 指定がなかったキーについてはnullを設定する
		bindParams.forEach(s -> ctx.param(s, null));
	}

	/**
	 * 1つのパラメータの設定
	 *
	 * パラメータ値は以下の表記が可能
	 * <dl>
	 * 	<dh>[NULL]</dh>
	 *  <dd><code>null</code>を設定する</dd>
	 * 	<dh>[EMPTY]</dh>
	 *  <dd>""（空文字）を設定する</dd>
	 *  <dh>'値'</dh>
	 *  <dd>文字列として設定する. 空白を含めることもできる</dd>
	 * 	<dh>[値1,値2,...]</dh>
	 *  <dd>配列として設定する</dd>
	 * 	<dh>その他</dh>
	 *  <dd>文字列として設定する</dd>
	 * </dl>
	 *
	 * @param ctx SqlContext
	 * @param key パラメータキー
	 * @param val パラメータ値
	 */
	private void setParam(final SqlContext ctx, final String key, final String val) {
		if (val.startsWith("[") && val.endsWith("]")) {
			// [] で囲まれた値は配列に変換する。ex) [1, 2] => {"1", "2"}
			String[] parts = val.substring(1, val.length() - 1).split("\\s*,\\s*");
			Object[] vals = new Object[parts.length];
			for (int i = 0; i < parts.length; i++) {
				vals[i] = convertSingleValue(parts[i]);
			}
			ctx.paramList(key, vals);
		} else {
			ctx.param(key, convertSingleValue(val));
		}
	}

	/**
	 * パラメータで渡された単独の値を型変換する
	 *
	 * @param val 値の文字列
	 * @return 変換後オブジェクト
	 */
	private Object convertSingleValue(final String val) {
		String value = StringUtils.trim(val);
		if (StringUtils.isEmpty(value)) {
			return null;
		} else if ("[NULL]".equalsIgnoreCase(value)) {
			return null;
		} else if ("[EMPTY]".equalsIgnoreCase(value)) {
			return "";
		} else if (value.startsWith("'") && value.endsWith("'")) {
			// ''で囲まれた値は文字列として扱う。空白を含むこともできる。 ex) 'This is a pen'
			return value.substring(1, value.length() - 1);
		} else if (Boolean.TRUE.toString().equalsIgnoreCase(value)) {
			return Boolean.TRUE;
		} else if (Boolean.FALSE.toString().equalsIgnoreCase(value)) {
			return Boolean.FALSE;
		} else if (NumberUtils.isCreatable(value)) {
			return NumberUtils.createNumber(value);
		} else {
			try {
				// 日時に変換できるか
				return new Timestamp(DateUtils.parseDateStrictly(value, "yyyy-MM-dd'T'HH:mm:ss").getTime());
			} catch (ParseException ex) {
				// do nothing
			}

			try {
				// 日付に変換できるか？
				return new java.sql.Date(DateUtils.parseDateStrictly(value, "yyyy-MM-dd").getTime());
			} catch (ParseException ex) {
				// do nothing
			}

			try {
				// 時刻に変換できるか？
				return new Time(DateUtils.parseDateStrictly(value, "HH:mm:ss").getTime());
			} catch (ParseException ex) {
				// do nothing
			}
			return value;
		}
	}

	/**
	 * SQLパラメータの解析
	 *
	 * @param sql 解析対象SQL
	 * @return SQLを解析して取得したパラメータキーのセット
	 */
	private Set<String> getSqlParams(final String sql) {
		SqlParser parser = new SqlParserImpl(sql);
		ContextTransformer transformer = parser.parse();
		Node rootNode = transformer.getRoot();

		Set<String> params = new LinkedHashSet<>();
		traverseNode(rootNode, params);

		params.removeIf(s -> CONSTANT_PAT.matcher(s).matches());

		return params;
	}

	/**
	 * SQLの探索
	 *
	 * @param node SQLノード
	 * @param params パラメータが見つかった場合に格納するSetオブジェクト
	 */
	private void traverseNode(final Node node, final Set<String> params) {
		if (node instanceof BindVariableNode) {
			params.add(((BindVariableNode) node).getExpression());
		} else if (node instanceof ParenBindVariableNode) {
			params.add(((ParenBindVariableNode) node).getExpression());
		} else if (node instanceof EmbeddedValueNode) {
			params.add(((EmbeddedValueNode) node).getExpression());
		} else if (node instanceof IfNode) {
			try {
				String expression = ((IfNode) node).getExpression();
				ognl.Node ognlNode = (ognl.Node) Ognl.parseExpression(expression);
				traverseExpression(ognlNode, params);
			} catch (OgnlException e) {
				e.printStackTrace();
			}
		}

		for (int i = 0; i < node.getChildSize(); i++) {
			Node childNode = node.getChild(i);
			traverseNode(childNode, params);
		}

	}

	/**
	 * 評価式の探索
	 *
	 * @param node SQLノード
	 * @param params パラメータが見つかった場合に格納するSetオブジェクト
	 */
	private void traverseExpression(final ognl.Node node, final Set<String> params) {
		if (node == null) {
			return;
		}
		if (node instanceof ASTProperty) {
			ASTProperty prop = (ASTProperty) node;
			params.add(prop.toString());
		} else {
			int childCount = node.jjtGetNumChildren();
			for (int i = 0; i < childCount; i++) {
				ognl.Node child = node.jjtGetChild(i);
				traverseExpression(child, params);
			}
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
	 * SQL Name の補完を行うCompleter
	 *
	 * @author H.Sugimoto
	 *
	 */
	class SqlNameCompleter implements Completer {
		private final SortedSet<String> sqlNames = new TreeSet<>();

		/**
		 * コンストラクタ
		 * @param sqlNames SQL名のリスト
		 */
		private SqlNameCompleter(final List<String> sqlNames) {
			this.sqlNames.addAll(sqlNames);
		}

		/**
		 * コンストラクタから渡されたSQL名の一覧と入力を比較し、前方一致するものがあれば補完対象とする<br>
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
			int startArgNo = len >= 1 ? Command.toCommand(parts[0]).getStartArgNo(this.getClass()) : -1;

			// 対象引数が-1の時は該当なしなのでコード補完しない
			if (startArgNo == -1) {
				return;
			} else {
				boolean isBlank = buffer.endsWith(" ");
				if (len == startArgNo && isBlank || len == startArgNo + 1 && !isBlank) {
					// コマンドが引数ありの場合
					String args = len == startArgNo + 1 ? parts[startArgNo] : "";
					if (StringUtils.isEmpty(args)) {
						candidates.addAll(sqlNames.stream().map(Candidate::new).collect(Collectors.toList()));
					} else {
						for (String match : sqlNames.tailSet(args)) {
							if (!match.startsWith(args)) {
								break;
							}
							candidates.add(new Candidate(match));
						}
					}
				}
			}
		}
	}

	/**
	 * バインドパラメータを補完するCompleter
	 *
	 * @author H.Sugimoto
	 *
	 */
	class BindParamCompleter implements Completer {
		/**
		 * バッファから渡されたSQL名のSQLを取得し、バインドパラメータをパースして取得する。<br>
		 * 取得したバインドパラメータと入力を比較し、前方一致する場合は補完候補にする。<br>
		 * ただし、すでに指定済みのバインドパラメータは補完候補から除く<br>
		 *
		 * {@inheritDoc}
		 *
		 * @see org.jline.reader.Completer#complete(org.jline.reader.LineReader, org.jline.reader.ParsedLine, java.util.List)
		 */
		@Override
		public void complete(final LineReader reader, final ParsedLine line, final List<Candidate> candidates) {
			String buffer = line.line();
			String[] parts = getLineParts(buffer);
			int pos = buffer.length();
			int len = parts.length;

			// コード補完する引数の番号を特定。
			int startArgNo = len >= 1 ? Command.toCommand(parts[0]).getStartArgNo(this.getClass()) : -1;

			// 対象引数が-1の時は該当なしなのでコード補完しない
			if (startArgNo == -1 || startArgNo >= len) {
				return;
			} else {
				boolean isBlank = buffer.endsWith(" ");
				if (len >= startArgNo) {
					// sqlNameが指定されている場合
					String sqlName = parts[startArgNo - 1];
					if (StringUtils.isNotEmpty(sqlName)) {
						String sql = config.getSqlManager().getSql(sqlName);
						if (StringUtils.isNotEmpty(sql)) {
							Set<String> params = getSqlParams(sql);
							if (len > startArgNo) {
								// 最後のパラメータ以外ですでに指定されたバインドパラメータを候補から除去する
								int lastPos = isBlank ? len : len - 1;
								for (int i = startArgNo; i < lastPos; i++) {
									String part = parts[i];
									String[] keyValue = part.split("=", 2);
									params.remove(keyValue[0]);
								}
								if (isBlank) {
									// 候補の表示位置を計算
									candidates.addAll(params.stream().map(Candidate::new).collect(Collectors.toList()));
								} else {
									// 候補の表示位置を計算
									pos = pos - parts[len - 1].length();
									// 最後のパラメータについて候補を作成
									String[] keyValue = parts[len - 1].split("=", 2);
									if (keyValue.length == 2) {
										// すでに値の入力があるため補完は行わない
										pos = -1;
									} else {
										String key = keyValue[0];
										for (String match : params) {
											if (match.startsWith(key)) {
												candidates.add(new Candidate(match));
											}
										}
									}
								}
							} else {
								candidates.addAll(params.stream().map(Candidate::new).collect(Collectors.toList()));
							}
						}
					}
				}
			}
		}
	}

	/**
	 * テーブル名を補完するCompleter
	 *
	 * @author H.Sugimoto
	 *
	 */
	class TableNameCompleter implements Completer {

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
			int startArgNo = len >= 1 ? Command.toCommand(parts[0]).getStartArgNo(this.getClass()) : -1;

			// 対象引数が-1の時は該当なしなのでコード補完しない
			if (startArgNo == -1) {
				return;
			} else {
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
					Connection conn = config.getConnectionSupplier().getConnection();
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
	}

	class SqlKeywordCompleter implements Completer {
		@Override
		public void complete(final LineReader reader, final ParsedLine line, final List<Candidate> candidates) {
			String buffer = line.line();
			String[] parts = getLineParts(buffer);
			int len = parts.length;

			// コード補完する引数の番号を特定。
			int startArgNo = len >= 1 ? Command.toCommand(parts[0]).getStartArgNo(this.getClass()) : -1;

			// 対象引数が-1の時は該当なしなのでコード補完しない
			if (startArgNo == -1) {
				return;
			} else {
				String key = null;

				boolean isBlank = buffer.endsWith(" ");
				if (len == startArgNo && isBlank) {
					key = null;
				} else if (len == startArgNo + 1 && !isBlank) {
					key = parts[len - 1];
				} else {
					return;
				}

				final String keyword = key;

				Stream.of("select", "insert", "update", "delete")
						.filter(c -> keyword == null || c.startsWith(keyword.toLowerCase()))
						.forEach(c -> candidates.add(new Candidate(c)));
			}
		}
	}

	/**
	 * 動的ロードしたDriverの中継用クラス
	 *
	 * DriverManagerはclasspathで指定したクラスしか実行しないため、中継用のクラス経由で動的にロードしたDriverを実行する
	 *
	 * @author H.Sugimoto
	 *
	 */
	class DriverShim implements Driver {
		private Driver driver = null;

		/**
		 * コンストラクタ
		 *
		 * @param driver 実Driver
		 */
		DriverShim(final Driver driver) {
			this.driver = driver;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see java.sql.Driver#connect(java.lang.String, java.util.Properties)
		 */
		@Override
		public Connection connect(final String url, final Properties info) throws SQLException {
			return driver.connect(url, info);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see java.sql.Driver#acceptsURL(java.lang.String)
		 */
		@Override
		public boolean acceptsURL(final String url) throws SQLException {
			return driver.acceptsURL(url);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see java.sql.Driver#getPropertyInfo(java.lang.String, java.util.Properties)
		 */
		@Override
		public DriverPropertyInfo[] getPropertyInfo(final String url, final Properties info) throws SQLException {
			return driver.getPropertyInfo(url, info);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see java.sql.Driver#getMajorVersion()
		 */
		@Override
		public int getMajorVersion() {
			return driver.getMajorVersion();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see java.sql.Driver#getMinorVersion()
		 */
		@Override
		public int getMinorVersion() {
			return driver.getMinorVersion();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see java.sql.Driver#jdbcCompliant()
		 */
		@Override
		public boolean jdbcCompliant() {
			return driver.jdbcCompliant();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see java.sql.Driver#getParentLogger()
		 */
		@Override
		public java.util.logging.Logger getParentLogger() throws SQLFeatureNotSupportedException {
			return driver.getParentLogger();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return driver.toString();
		}
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
