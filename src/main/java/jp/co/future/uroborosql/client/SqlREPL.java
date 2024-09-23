/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.JarURLConnection;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Driver;
import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.ServiceLoader;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

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
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.client.command.ReplCommand;
import jp.co.future.uroborosql.client.completer.BindParamCompleter;
import jp.co.future.uroborosql.client.completer.ReplCommandCompleter;
import jp.co.future.uroborosql.client.completer.SqlKeywordCompleter;
import jp.co.future.uroborosql.client.completer.SqlNameCompleter;
import jp.co.future.uroborosql.client.completer.TableNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.event.subscriber.DumpResultEventSubscriber;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.log.support.ReplLoggingSupport;
import jp.co.future.uroborosql.store.SqlInfo;
import jp.co.future.uroborosql.store.SqlResourceManagerImpl;
import jp.co.future.uroborosql.utils.ObjectUtils;

/**
 * SQL REPL実装クラス
 *
 * SQLの実行をREPL形式で行うことができる
 *
 * @author H.Sugimoto
 */
public class SqlREPL implements ReplLoggingSupport {
	/** プロパティ上のクラスパスに指定された環境変数を置換するための正規表現 */
	private static final Pattern SYSPROP_PAT = Pattern.compile("\\$\\{(.+?)\\}");
	/** プロパティパス */
	private final Path propPath;

	/** プロパティ */
	private final Properties props;

	/** 追加で読み込むクラスローダ */
	private URLClassLoader additionalClassLoader = null;

	/** SQL設定クラス */
	private SqlConfig sqlConfig = null;

	private final List<ReplCommand> commands = new ArrayList<>();

	/**
	 * メインメソッド
	 *
	 * @param args 読み込むプロパティファイルのファイルパス
	 */
	public static void main(final String... args) {
		((Logger) LoggerFactory.getLogger("jp.co.future.uroborosql.log")).setLevel(Level.INFO);
		((Logger) LoggerFactory.getLogger("jp.co.future.uroborosql.setting")).setLevel(Level.ERROR);
		((Logger) LoggerFactory.getLogger("jp.co.future.uroborosql.performance")).setLevel(Level.INFO);
		((Logger) LoggerFactory.getLogger("jp.co.future.uroborosql.event")).setLevel(Level.DEBUG);
		((Logger) LoggerFactory.getLogger("jp.co.future.uroborosql.repl")).setLevel(Level.WARN);
		((Logger) LoggerFactory.getLogger("jp.co.future.uroborosql.sql")).setLevel(Level.DEBUG);
		((Logger) LoggerFactory.getLogger("jp.co.future.uroborosql.sql.parser")).setLevel(Level.ERROR);
		((Logger) LoggerFactory.getLogger("jp.co.future.uroborosql.sql.coverage")).setLevel(Level.ERROR);

		var propFile = "repl.properties";
		if (args.length != 0) {
			propFile = args[0];
		}

		var path = Paths.get(propFile);

		if (!Files.exists(path)) {
			throw new IllegalArgumentException("properties could not found.");
		}

		try {
			var repl = new SqlREPL(path);
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
		var props = new Properties();
		try {
			var is = Files.newInputStream(path);
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

		// ReplCommandの読み込み
		for (var command : ServiceLoader.load(ReplCommand.class)) {
			commands.add(command);
		}
	}

	/**
	 * REPLの実行
	 *
	 * @throws Exception 実行時例外
	 */
	private void execute() throws Exception {
		try (var terminal = TerminalBuilder.builder().build()) {
			showHelp(terminal);
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

		var currentClassLoader = Thread.currentThread().getContextClassLoader();

		if (currentClassLoader.equals(additionalClassLoader)) {
			// すでに追加クラスローダになっている場合はいったん親に移動しておく
			currentClassLoader = additionalClassLoader.getParent();
		}

		var paths = p("sql.additionalClassPath", ".");
		var urls = new ArrayList<URL>();
		Arrays.stream(paths.split(";")).forEach(path -> {
			try {
				var m = SYSPROP_PAT.matcher(path);
				var sb = new StringBuilder();
				while (m.find()) {
					var key = m.group(1);
					var val = System.getProperty(key, null);
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
			} catch (Exception ex) {
				errorWith(REPL_LOG)
						.setMessage(ex.getMessage())
						.setCause(ex)
						.log();
			}
		});
		additionalClassLoader = new URLClassLoader(urls.toArray(new URL[urls.size()]), currentClassLoader);

		Thread.currentThread().setContextClassLoader(additionalClassLoader);

		var loader = ServiceLoader.load(Driver.class, additionalClassLoader);
		loader.forEach(driver -> {
			try {
				DriverManager.registerDriver(new DriverShim(driver));
			} catch (Exception ex) {
				errorWith(REPL_LOG)
						.setMessage(ex.getMessage())
						.setCause(ex)
						.log();
			}
		});

		var url = p("db.url", "");
		var user = p("db.user", "");
		var password = p("db.password", "");
		var schema = p("db.schema", null);
		var loadPath = p("sql.loadPath", "sql");
		var fileExtension = p("sql.fileExtension", ".sql");
		var charset = Charset.forName(p("sql.encoding", "UTF-8"));

		// config
		sqlConfig = UroboroSQL.builder(url, user, password, schema)
				.setSqlResourceManager(new SqlResourceManagerImpl(loadPath, fileExtension, charset))
				.build();
		sqlConfig.getEventListenerHolder().addEventSubscriber(new DumpResultEventSubscriber());

		// executionContextProvider
		var executionContextProvider = sqlConfig.getExecutionContextProvider();
		var constantClassNames = Arrays
				.asList(p("executionContextProvider.constantClassNames", "").split("\\s*,\\s*")).stream()
				.filter(ObjectUtils::isNotEmpty)
				.collect(Collectors.toList());
		if (!constantClassNames.isEmpty()) {
			executionContextProvider.setConstantClassNames(constantClassNames);
		}

		var enumConstantPackageNames = Arrays
				.asList(p("executionContextProvider.enumConstantPackageNames", "").split("\\s*,\\s*")).stream()
				.filter(ObjectUtils::isNotEmpty)
				.collect(Collectors.toList());
		if (!enumConstantPackageNames.isEmpty()) {
			executionContextProvider.setEnumConstantPackageNames(enumConstantPackageNames);
		}

		if (!constantClassNames.isEmpty() || !enumConstantPackageNames.isEmpty()) {
			executionContextProvider.initialize();
		}
		loadAllSqlNames(Paths.get(loadPath), fileExtension);
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
	}

	private void loadAllSqlNames(final Path rootPath, final String fileExtension) {
		Thread.currentThread().getContextClassLoader()
				.resources(rootPath.toString().replace('\\', '/'))
				.flatMap(url -> {
					try {
						var scheme = url.toURI().getScheme();
						if (SqlInfo.SCHEME_FILE.equalsIgnoreCase(scheme)) {
							return traverseFile(url, rootPath, fileExtension).stream();
						} else if (SqlInfo.SCHEME_JAR.equalsIgnoreCase(scheme)) {
							return traverseJar(url, rootPath, fileExtension).stream();
						}
						return null;
					} catch (IOException | URISyntaxException ex) {
						errorWith(REPL_LOG)
								.setMessage("Can't load sql files.")
								.setCause(ex)
								.log();
						throw new UroborosqlRuntimeException("I/O error occurred.", ex);
					}
				})
				.filter(Objects::nonNull)
				.map(sqlName -> sqlConfig.getSqlResourceManager().getSql(sqlName))
				.count();
	}

	/**
	 * 指定されたURL配下のファイルを順次追跡し、SQL名のリストを取得する.
	 *
	 * @param url 追跡を行うディレクトリ、またはファイルのURL
	 * @param rootPath 読み込みを行ったSQLルートパス
	 * @return PathとSQL本文を格納したMap
	 * @param fileExtension ファイル拡張子
	 * @throws IOException SQLの読み込みに失敗した場合
	 */
	private List<String> traverseFile(final URL url, final Path rootPath, final String fileExtension)
			throws IOException {
		try {
			var path = Path.of(url.toURI());
			if (Files.notExists(path)) {
				List.of();
			}
			var sqlNames = new ArrayList<String>();
			if (Files.isDirectory(path)) {
				try (var ds = Files.newDirectoryStream(path)) {
					for (var child : ds) {
						sqlNames.addAll(traverseFile(child.toUri().toURL(), rootPath, fileExtension));
					}
				} catch (IOException ex) {
					throw new UroborosqlRuntimeException("I/O error occurred.", ex);
				}
			} else if (path.toString().endsWith(fileExtension)) {
				sqlNames.add(sqlConfig.getSqlResourceManager().getSqlName(path));
			}
			return sqlNames;
		} catch (URISyntaxException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * 指定されたjarのURL配下のファイルを順次追跡し、SQL名のリストを取得する.
	 *
	 * @param url 追跡を行うディレクトリ、またはファイルのURL
	 * @param rootPath 読み込みを行ったSQLルートパス
	 * @param fileExtension ファイル拡張子
	 * @return PathとSQL本文を格納したMap
	 * @throws IOException SQLの読み込みに失敗した場合
	 */
	private List<String> traverseJar(final URL url, final Path rootPath, final String fileExtension)
			throws IOException {
		var conn = (JarURLConnection) url.openConnection();
		try (var jarFile = conn.getJarFile()) {
			return jarFile.stream()
					.map(jarEntry -> {
						var name = jarEntry.getName();
						if (!jarEntry.isDirectory() && name.endsWith(fileExtension)) {
							var path = Paths.get(name);
							return sqlConfig.getSqlResourceManager().getSqlName(path);
						} else {
							return null;
						}
					})
					.filter(Objects::nonNull)
					.collect(Collectors.toList());
		}
	}

	/**
	 * コンソールからの入力のリスン
	 *
	 * @param terminal Terminal
	 * @throws IOException I/O例外
	 */
	private void listen(final Terminal terminal) throws IOException {
		var completers = new ArrayList<Completer>();

		// コマンドのコード補完
		completers.add(new ReplCommandCompleter(commands));
		// SQL名のコード補完
		completers.add(new SqlNameCompleter(commands, sqlConfig.getSqlResourceManager()));
		// バインドパラメータのコード補完
		completers.add(new BindParamCompleter(commands, sqlConfig));
		// テーブル名のコード補完
		completers.add(new TableNameCompleter(commands, sqlConfig.getConnectionSupplier()));
		// SQLキーワードのコード補完
		completers.add(new SqlKeywordCompleter(commands));

		var reader = LineReaderBuilder.builder()
				.appName("uroborosql")
				.terminal(terminal)
				.completer(new AggregateCompleter(completers))
				.highlighter(new DefaultHighlighter())
				.option(LineReader.Option.CASE_INSENSITIVE, true)
				.build();
		while (true) { // ユーザの一行入力を待つ
			try {
				if (!executeCommand(reader)) {
					break;
				}
			} catch (UserInterruptException | EndOfFileException ex) {
				break;
			} catch (Exception ex) {
				errorWith(REPL_LOG)
						.setMessage(ex.getMessage())
						.setCause(ex)
						.log();
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
	private boolean executeCommand(final LineReader reader) throws Exception {
		var prompt = new AttributedStringBuilder()
				.style(AttributedStyle.BOLD.foreground(AttributedStyle.GREEN))
				.ansiAppend("uroborosql")
				.style(AttributedStyle.DEFAULT)
				.append(" > ")
				.toAnsi();
		var line = reader.readLine(prompt);
		if (line == null || line.isEmpty()) {
			// 空なら何もせずにループ
			return true;
		}

		var parts = SqlParamUtils.parseLine(line);
		return commands.stream()
				.filter(c -> c.is(parts[0]))
				.findFirst()
				.map(cmd -> cmd.execute(reader, parts, sqlConfig, props))
				.orElseGet(() -> {
					showHelp(reader.getTerminal());
					return true;
				});
	}

	/**
	 * HELPメッセージの表示
	 * @param terminal Terminal
	 */
	private void showHelp(final Terminal terminal) {
		showMessage(terminal, "/message.txt");
		commands.stream().filter(c -> !c.isHidden()).forEach(c -> c.showHelp(terminal));
	}

	/**
	 * メッセージの表示
	 * @throws IOException IO例外
	 */
	private void showMessage(final Terminal terminal, final String path) {
		var messageFilePath = this.getClass().getPackage().getName().replace(".", "/") + path;
		try (var reader = new BufferedReader(new InputStreamReader(Thread.currentThread()
				.getContextClassLoader().getResourceAsStream(messageFilePath), Charset.forName("UTF-8")))) {
			reader.lines().forEach(s -> {
				try {
					terminal.writer().println(s);
				} catch (Exception ex) {
					// ここで例外が出てもメッセージ表示が正しく出ないだけなので、エラーを握りつぶす
				}
			});
		} catch (IOException ex) {
			// ここで例外が出てもメッセージ表示が正しく出ないだけなので、エラーを握りつぶす
		}
		terminal.flush();
	}

	/**
	 * 現在読み込んでいるプロパティの情報を表示
	 * @throws IOException IO例外
	 */
	private void showProps(final Terminal terminal) throws IOException {
		var writer = terminal.writer();
		writer.println("Properties file path:" + this.propPath);
		writer.println("[Properties]");
		props.forEach((key, value) -> {
			try {
				writer.println(key + "=" + value);
			} catch (Exception ex) {
				// ここで例外が出てもメッセージ表示が正しく出ないだけなので、エラーを握りつぶす
			}
		});
		writer.println();
		terminal.flush();
	}

}
