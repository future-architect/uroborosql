package jp.co.future.uroborosql.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.DriverPropertyInfo;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Properties;
import java.util.ServiceLoader;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import jline.console.ConsoleReader;
import jline.console.UserInterruptException;
import jline.console.completer.CandidateListCompletionHandler;
import jline.console.completer.Completer;
import jline.console.completer.StringsCompleter;
import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.filter.DumpResultSqlFilter;
import jp.co.future.uroborosql.node.BindVariableNode;
import jp.co.future.uroborosql.node.EmbeddedValueNode;
import jp.co.future.uroborosql.node.IfNode;
import jp.co.future.uroborosql.node.Node;
import jp.co.future.uroborosql.node.ParenBindVariableNode;
import jp.co.future.uroborosql.parser.ContextTransformer;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;
import ognl.ASTProperty;
import ognl.Ognl;
import ognl.OgnlException;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;

/**
 * SQL REPL実装クラス
 *
 * SQLの実行をREPL形式で行うことができる
 *
 * @author H.Sugimoto
 */
public class SqlREPL {
	/** バインドパラメータ中の定数指定を判定するための正規表現 */
	private static final Pattern CONSTANT_PAT = Pattern.compile("^[A-Z][A-Z_-]*$");

	/** プロパティ上のクラスパスに指定された環境変数を置換するための正規表現 */
	private static final Pattern SYSPROP_PAT = Pattern.compile("\\$\\{(.+?)\\}");

	/** コマンド名 */
	private enum Command {
		query(true), update(true), list(false), reload(false), driver(false), help(false), cls(false), exit(false);

		private boolean hasArgs = false;

		private Command(final boolean hasArgs) {
			this.hasArgs = hasArgs;
		}

		public boolean hasArgs() {
			return hasArgs;
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
		showMessage("/message.txt");
		showProps();

		initialize();

		listen();
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

		config = DefaultSqlConfig.getConfig(p("db.url", ""), p("db.user", ""), p("db.password", ""),
				p("db.schema", null), p("sql.loadPath", "sql"));

		config.getSqlFilterManager().addSqlFilter(new DumpResultSqlFilter());
		config.getSqlFilterManager().initialize();

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

	private void listen() throws IOException {
		ConsoleReader console = new ConsoleReader();
		console.setPrompt("uroborosql > ");
		console.setHandleUserInterrupt(true);

		// コマンドのコード補完
		console.addCompleter(new StringsCompleter(Arrays.asList(Command.values()).stream().map(c -> c.toString())
				.collect(Collectors.toList())));
		// SQL名のコード補完
		console.addCompleter(new SqlNameCompleter(config.getSqlManager().getSqlPathList()));
		// バインドパラメータのコード補完
		console.addCompleter(new BindParamCompleter());
		CandidateListCompletionHandler handler = new CandidateListCompletionHandler();
		handler.setPrintSpaceAfterFullCompletion(false);
		console.setCompletionHandler(handler);

		while (true) { // ユーザの一行入力を待つ
			try {
				String line = console.readLine();
				if (!commandExecute(line, console)) {
					break;
				}
			} catch (UserInterruptException uie) {
			} catch (Exception e) {
				e.printStackTrace(System.err);
			}
		}
	}

	/**
	 * 入力から指定されたコマンドを判定し実行する。
	 *
	 * @param line 入力文字列
	 * @param console Inputスキャナ
	 * @return 入力を継続する場合は<code>true</code>
	 * @throws Exception 実行時例外
	 */
	private boolean commandExecute(final String line, final ConsoleReader console) throws Exception {
		if (line == null || line.isEmpty()) {
			// 空なら何もせずにループ
			return true;
		}

		String[] parts = line.split(" ");
		String command = parts[0].toUpperCase();

		switch (command) {
		case "EXIT":
			System.out.println("SQL REPL end.");
			return false;

		case "RELOAD":
			System.out.println("RELOAD " + propPath);

			for (Enumeration<Driver> drivers = DriverManager.getDrivers(); drivers.hasMoreElements();) {
				Driver driver = drivers.nextElement();
				if (driver instanceof DriverShim) {
					DriverManager.deregisterDriver(driver);
				}
			}
			props = loadProps(propPath);
			showProps();
			initialize();

			return true;

		case "LIST":
			System.out.println("LIST:");
			int sqlCount = 0;
			List<String> pathList = config.getSqlManager().getSqlPathList();
			int numSize = String.valueOf(pathList.size()).length();
			for (String key : pathList) {
				System.out.printf("%0" + numSize + "d : %s%n", ++sqlCount, key);
			}

			return true;

		case "DRIVER":
			System.out.println("DRIVER:");
			Enumeration<Driver> drivers = DriverManager.getDrivers();
			int driverCount = 0;
			while (drivers.hasMoreElements()) {
				Driver driver = drivers.nextElement();
				System.out.printf("%02d : %s%n", ++driverCount, driver);
			}

			return true;

		case "HELP":
			System.out.println("HELP:");
			showMessage("/message.txt");

			return true;

		case "THIS":
			showMessage("/this.txt");

			return true;

		case "CLS":
			console.clearScreen();
			return true;

		case "QUERY":
			if (parts.length >= 2) {
				String sqlName = parts[1];
				if (config.getSqlManager().existSql(sqlName)) {

					try (SqlAgent agent = config.createAgent()) {
						SqlContext ctx = agent.contextFrom(sqlName.replaceAll("\\.", "/"));
						String[] params = Arrays.copyOfRange(parts, 2, parts.length);
						setSqlParams(console, ctx, params);

						ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

						try (ResultSet rs = agent.query(ctx)) {
							System.out.println("query sql[" + sqlName + "] end.");
						} finally {
							agent.rollback();
						}
					}
				} else {
					System.out.println("SQL not found. sql=" + sqlName);
				}
			}
			return true;

		case "UPDATE":
			if (parts.length >= 2) {
				String sqlName = parts[1];
				if (config.getSqlManager().existSql(sqlName)) {

					try (SqlAgent agent = config.createAgent()) {
						SqlContext ctx = agent.contextFrom(sqlName.replaceAll("\\.", "/"));
						String[] params = Arrays.copyOfRange(parts, 2, parts.length);
						setSqlParams(console, ctx, params);
						try {
							int ans = agent.update(ctx);
							agent.commit();
							System.out.println("update sql[" + sqlName + "] end. row count=" + ans);
						} catch (SQLException ex) {
							agent.rollback();
						}
					}
				} else {
					System.out.println("SQL not found. sql=" + sqlName);
				}
			}
			return true;

		default:
			return true;
		}
	}

	/**
	 * @param console
	 * @param ctx
	 * @param paramsArray
	 * @throws IOException
	 */
	private void setSqlParams(final ConsoleReader console, final SqlContext ctx, final String[] paramsArray)
			throws IOException {
		if (paramsArray.length > 0) {
			for (String element : paramsArray) {
				String[] param = element.split("=");
				String key = param[0];
				if (param.length == 1) {
					// キーだけの指定は値をnullと扱う
					ctx.param(key, null);
				} else {
					String val = param[1];
					setParam(ctx, key, val);
				}
			}
		} else {
			Set<String> params = getSqlParams(config.getSqlManager().getSql(ctx.getSqlName()));
			for (String key : params) {
				console.putString(" >> param : " + key + "=");
				console.flush();
				String val = console.readLine();
				setParam(ctx, key, val);
			}
		}
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
			if (StringUtils.isNotBlank(val)) {
				ctx.param(key, convertSingleValue(val));
			}
		}
	}

	/**
	 * パラメータで渡された単独の値を型変換する
	 * @param val 値の文字列
	 * @return 変換後オブジェクト
	 */
	private Object convertSingleValue(final String val) {
		if ("[NULL]".equalsIgnoreCase(val)) {
			return null;
		} else if ("[EMPTY]".equalsIgnoreCase(val)) {
			return "";
		} else if (Boolean.TRUE.toString().equalsIgnoreCase(val)) {
			return Boolean.TRUE;
		} else if (Boolean.FALSE.toString().equalsIgnoreCase(val)) {
			return Boolean.FALSE;
		} else if (NumberUtils.isDigits(val)) {
			return NumberUtils.toInt(val);
		} else {
			return val;
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

		Set<String> params = new LinkedHashSet<String>();
		traverseNode(rootNode, params);

		params.removeIf(s -> CONSTANT_PAT.matcher(s).matches());

		return params;
	}

	/**
	 * SQLの探索
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
	 */
	private void showMessage(final String path) {
		String messageFilePath = this.getClass().getPackage().getName().replace(".", "/") + path;
		try (BufferedReader reader = new BufferedReader(new InputStreamReader(Thread.currentThread()
				.getContextClassLoader().getResourceAsStream(messageFilePath), Charset.forName("UTF-8")))) {
			reader.lines().forEach(s -> System.out.println(s));
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}

	/**
	 * 現在読み込んでいるプロパティの情報を表示
	 */
	private void showProps() {
		System.out.println("[Properties]");
		props.forEach((key, value) -> {
			System.out.println(key + "=" + value);
		});
		System.out.println("");
	}

	class SqlNameCompleter implements Completer {
		private final SortedSet<String> sqlNames = new TreeSet<String>();

		private SqlNameCompleter(final List<String> sqlNames) {
			this.sqlNames.addAll(sqlNames);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jline.console.completer.Completer#complete(java.lang.String, int, java.util.List)
		 */
		@Override
		public int complete(final String buffer, final int cursor, final List<CharSequence> candidates) {
			String[] parts = buffer.split(" ");
			boolean isBlank = buffer.endsWith(" ");
			int len = parts.length;
			if (len == 1 || (len == 2 && !isBlank)) {
				try {
					if (Command.valueOf(parts[0].toLowerCase()).hasArgs()) {
						// コマンドが引数ありの場合
						String args = len == 2 ? parts[1] : "";
						if (StringUtils.isEmpty(args)) {
							candidates.addAll(sqlNames);
						} else {
							for (String match : sqlNames.tailSet(args)) {
								if (!match.startsWith(args)) {
									break;
								}
								candidates.add(match);
							}
						}
					}
				} catch (Exception ex) {
					// do nothing
				}
				return candidates.isEmpty() ? buffer.length() : parts[0].length() + 1;
			} else {
				return -1;
			}
		}
	}

	class BindParamCompleter implements Completer {
		/**
		 * {@inheritDoc}
		 *
		 * @see jline.console.completer.Completer#complete(java.lang.String, int, java.util.List)
		 */
		@Override
		public int complete(final String buffer, final int cursor, final List<CharSequence> candidates) {
			String[] parts = buffer.split(" ");
			int pos = buffer.length();
			int len = parts.length;
			boolean isBlank = buffer.endsWith(" ");
			if (len >= 2) {
				try {
					if (Command.valueOf(parts[0].toLowerCase()).hasArgs()) {
						// sqlNameが指定されている場合
						String sqlName = parts[1];
						if (StringUtils.isNotEmpty(sqlName)) {
							String sql = config.getSqlManager().getSql(sqlName);
							if (StringUtils.isNotEmpty(sql)) {
								Set<String> params = getSqlParams(sql);
								if (len >= 3) {
									// 最後のパラメータ以外ですでに指定されたバインドパラメータを候補から除去する
									int lastPos = isBlank ? len : len - 1;
									for (int i = 2; i < lastPos; i++) {
										String part = parts[i];
										String[] keyValue = part.split("=", 2);
										if (keyValue.length == 2) {
											params.remove(keyValue[0]);
										}
									}
									if (isBlank) {
										// 候補の表示位置を計算
										candidates.addAll(params);
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
													candidates.add(match);
												}
											}
										}
									}
								} else {
									candidates.addAll(params);
								}
							}
						}
					}
				} catch (Exception ex) {
					// do nothing
				}
				return pos;
			} else {
				return -1;
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

}
