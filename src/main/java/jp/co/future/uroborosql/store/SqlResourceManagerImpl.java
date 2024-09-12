/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import static java.nio.file.StandardWatchEventKinds.ENTRY_CREATE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_DELETE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_MODIFY;
import static java.nio.file.StandardWatchEventKinds.OVERFLOW;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.JarURLConnection;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.FileSystem;
import java.nio.file.FileSystemNotFoundException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.log.support.ServiceLoggingSupport;
import jp.co.future.uroborosql.utils.ObjectUtils;

/**
 * SQLリソース管理実装クラス
 *
 * @author H.Sugimoto
 */
public class SqlResourceManagerImpl implements SqlResourceManager {
	/** zip, jar内のファイルのscheme */
	private static final String SCHEME_JAR = "jar";
	/** ファイルシステム上のファイルのscheme */
	private static final String SCHEME_FILE = "file";

	/** SQLファイルロードのデフォルトルートパス */
	private static final String DEFAULT_LOAD_PATH = "sql";

	/** 有効なDialectのSet */
	private static final Set<String> dialects = StreamSupport
			.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
			.map(Dialect::getDatabaseType)
			.collect(Collectors.toSet());

	/** SQLファイルをロードするルートパスのリスト */
	private final List<Path> loadPaths;

	/** SQLファイルをロードするルートパスの各階層を保持する配列（ルートパス特定用） */
	private final List<String[]> loadPathPartsList;

	/** SQLファイル拡張子 */
	private final String fileExtension;

	/** SQLファイルエンコーディング */
	private final Charset charset;

	/** SQLファイルの変更を検知するかどうか */
	private final boolean detectChanges;

	/** Dialect */
	private Dialect dialect;

	/** SQLファイル監視サービス */
	private WatchService watcher;

	/** SQLファイル監視サービスの実行サービス */
	private ExecutorService es;

	/** sqlNameとそれに対するSqlInfoの紐付きを持つMap */
	protected final ConcurrentHashMap<String, SqlInfo> sqlInfos = new ConcurrentHashMap<>();

	/** WatchKeyに対するディレクトリPathを取得するためのMap */
	protected final ConcurrentHashMap<WatchKey, Path> watchDirs = new ConcurrentHashMap<>();

	/**
	 * コンストラクタ
	 */
	public SqlResourceManagerImpl() {
		this(DEFAULT_LOAD_PATH);
	}

	/**
	 * コンストラクタ
	 *
	 * @param detectChanges SQLファイルの変更を検知するかどうか
	 */
	public SqlResourceManagerImpl(final boolean detectChanges) {
		this(DEFAULT_LOAD_PATH, null, null, detectChanges);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 */
	public SqlResourceManagerImpl(final String loadPath) {
		this(loadPath, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param fileExtension SQLファイル拡張子
	 */
	public SqlResourceManagerImpl(final String loadPath, final String fileExtension) {
		this(loadPath, fileExtension, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 */
	public SqlResourceManagerImpl(final String loadPath, final String fileExtension, final Charset charset) {
		this(loadPath, fileExtension, charset, false);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 * @param detectChanges SQLファイルの変更を検知するかどうか
	 */
	public SqlResourceManagerImpl(final String loadPath, final String fileExtension, final Charset charset,
			final boolean detectChanges) {
		this(List.of(loadPath), fileExtension, charset, detectChanges);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLファイルをロードするルートパスのリスト
	 */
	public SqlResourceManagerImpl(final List<String> loadPaths) {
		this(loadPaths, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLファイルをロードするルートパスのリスト
	 * @param fileExtension SQLファイル拡張子
	 */
	public SqlResourceManagerImpl(final List<String> loadPaths, final String fileExtension) {
		this(loadPaths, fileExtension, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLファイルをロードするルートパスのリスト
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 */
	public SqlResourceManagerImpl(final List<String> loadPaths, final String fileExtension, final Charset charset) {
		this(loadPaths, fileExtension, charset, false);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLファイルをロードするルートパスのリスト
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 * @param detectChanges SQLファイルの変更を検知するかどうか
	 *
	 * @throws IllegalArgumentException loadPathsに<code>null</code>が含まれる場合
	 */
	public SqlResourceManagerImpl(final List<String> loadPaths, final String fileExtension, final Charset charset,
			final boolean detectChanges) {
		this.loadPaths = new ArrayList<>();
		for (var loadPath : loadPaths) {
			if (loadPath == null) {
				throw new IllegalArgumentException("loadPath is required.");
			}
			this.loadPaths.add(Paths.get(loadPath));
		}
		this.loadPathPartsList = new ArrayList<>();
		for (var loadPath : this.loadPaths) {
			var pathList = new ArrayList<String>();
			for (var part : loadPath) {
				pathList.add(part.toString());
			}
			this.loadPathPartsList.add(pathList.toArray(new String[pathList.size()]));
		}
		this.fileExtension = fileExtension != null ? fileExtension : ".sql";
		this.charset = charset != null ? charset : Charset.forName(Charset.defaultCharset().displayName());
		this.detectChanges = detectChanges;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#initialize()
	 */
	@Override
	public void initialize() {
		if (detectChanges) {
			try {
				watcher = FileSystems.getDefault().newWatchService();
			} catch (IOException ex) {
				errorWith(LOG)
						.setMessage("Can't start watcher service.")
						.setCause(ex)
						.log();
				return;
			}
		}

		generateSqlInfos();

		if (detectChanges) {
			// Path監視用のスレッド実行
			es = Executors.newSingleThreadExecutor();
			es.execute(this::watchPath);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#shutdown()
	 */
	@Override
	public void shutdown() {
		if (detectChanges) {
			es.shutdown();
			try {
				if (!es.awaitTermination(1000, TimeUnit.MILLISECONDS)) {
					es.shutdownNow();
				}
			} catch (InterruptedException ex) {
				es.shutdownNow();
			}
		}
	}

	/**
	 * Pathの監視
	 */
	private void watchPath() {
		for (;;) {
			//監視キーの送信を待機
			WatchKey key;
			try {
				key = watcher.take();
			} catch (InterruptedException ex) {
				debugWith(LOG)
						.log("WatchService caught InterruptedException.");
				break;
			} catch (Throwable ex) {
				errorWith(LOG)
						.setMessage("Unexpected exception occurred.")
						.setCause(ex)
						.log();
				break;
			}

			for (var event : key.pollEvents()) {
				var kind = event.kind();

				if (kind == OVERFLOW) {
					continue;
				}

				//ファイル名はイベントのコンテキストです。
				@SuppressWarnings("unchecked")
				var evt = (WatchEvent<Path>) event;
				var dir = watchDirs.get(key);
				var path = dir.resolve(evt.context());

				debugWith(LOG)
						.setMessage("file changed.({}). path={}")
						.addArgument(kind.name())
						.addArgument(path)
						.log();
				var isSqlFile = path.toString().endsWith(fileExtension);
				if (Files.isDirectory(path) || !isSqlFile) {
					// ENTRY_DELETEの時はFiles.isDirectory()がfalseになるので拡張子での判定も行う
					if (kind == ENTRY_CREATE) {
						traverseFile(path, true, false);
					} else if (kind == ENTRY_DELETE) {
						key.cancel();
						watchDirs.remove(key);
						continue;
					}
				} else if (isSqlFile) {
					if (kind == ENTRY_CREATE) {
						traverseFile(path, true, false);
					} else if (kind == ENTRY_MODIFY || kind == ENTRY_DELETE) {
						var sqlName = getSqlName(path);
						sqlInfos.computeIfPresent(sqlName, (k, v) -> v.computePath(path, kind == ENTRY_DELETE));
					}
				}
			}
			key.reset();
		}
		try {
			watcher.close();
		} catch (IOException ex) {
			// do nothing
		}
	}

	/**
	 * charset を取得します。
	 *
	 * @return charset
	 */
	public Charset getCharset() {
		return charset;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#getDialect()
	 */
	@Override
	public Dialect getDialect() {
		return dialect;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#setDialect(jp.co.future.uroborosql.dialect.Dialect)
	 */
	@Override
	public void setDialect(final Dialect dialect) {
		this.dialect = dialect;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#getSqlPathList()
	 */
	@Override
	public List<String> getSqlPathList() {
		var list = Collections.list(this.sqlInfos.keys());
		Collections.sort(list);
		return list;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#existSql(java.lang.String)
	 */
	@Override
	public boolean existSql(final String sqlName) {
		return sqlInfos.containsKey(sqlName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#getSql(java.lang.String)
	 */
	@Override
	public String getSql(final String sqlName) {
		if (existSql(sqlName)) {
			return sqlInfos.get(sqlName).getSqlBody();
		} else {
			throw new UroborosqlRuntimeException("sql file not found. sqlName : " + sqlName);
		}
	}

	/**
	 * sqlNameとそれに対するSqlInfoのMapを生成する
	 */
	protected void generateSqlInfos() {
		try {
			for (var loadPath : this.loadPaths) {
				var loadPathSlash = loadPath.toString().replace('\\', '/');
				var root = Thread.currentThread().getContextClassLoader().getResources(loadPathSlash);

				while (root.hasMoreElements()) {
					var url = root.nextElement();
					var scheme = url.toURI().getScheme();
					if (SCHEME_FILE.equalsIgnoreCase(scheme)) {
						traverseFile(Paths.get(url.toURI()), detectChanges && true, false);
					} else if (SCHEME_JAR.equalsIgnoreCase(scheme)) {
						traverseJar(url, loadPathSlash);
					} else {
						warnWith(LOG)
								.setMessage("Unsupported scheme. scheme : {}, url : {}")
								.addArgument(scheme)
								.addArgument(url)
								.log();
					}
				}
			}
		} catch (IOException | URISyntaxException ex) {
			errorWith(LOG)
					.setMessage("Can't load sql files.")
					.setCause(ex)
					.log();
		}
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#getSqlName(java.nio.file.Path)
	 */
	@Override
	public String getSqlName(final Path path) {
		var builder = new StringBuilder();

		var dialectFlag = true;
		for (var part : relativePath(path)) {
			var s = part.toString();
			if (dialectFlag) {
				// loadPathの直下がdialectと一致する場合はその下のフォルダから名前を付ける
				dialectFlag = false;
				if (dialects.contains(s.toLowerCase())) {
					continue;
				}
			}
			builder.append(s).append("/");
		}

		return builder.substring(0, builder.length() - (fileExtension.length() + 1));
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#getSqlPath(java.lang.String)
	 */
	@Override
	public Path getSqlPath(final String sqlName) {
		if (existSql(sqlName)) {
			return sqlInfos.get(sqlName).getPath();
		} else {
			throw new UroborosqlRuntimeException("sql file not found. sqlName : " + sqlName);
		}
	}

	/**
	 * loadPathからの相対パスを取得する.
	 * loadPathと一致する部分がなかった場合は、引数のpathの値をそのまま返却する
	 *
	 * @param path 相対パスを取得する元のパス
	 * @return 相対パス
	 */
	private Path relativePath(final Path path) {
		var pathList = new ArrayList<Path>();
		for (var part : path) {
			pathList.add(part);
		}

		for (var loadPathParts : this.loadPathPartsList) {
			var loadPathSize = loadPathParts.length;

			// loadPathのフォルダの並びと一致する場所を特定し、その下を相対パスとして返却する
			for (var i = 0; i < pathList.size() - loadPathSize; i++) {
				var paths = pathList.subList(i, i + loadPathSize).stream()
						.map(Path::toString)
						.toArray(String[]::new);
				if (Arrays.equals(loadPathParts, paths)) {
					return path.subpath(i + loadPathSize, path.getNameCount());
				}
			}
		}
		// loadPathと一致しなかった場合は元のpathを返却する
		return path;
	}

	/**
	 * 引数で指定したパスがDialect指定なし、または現在のDialect指定に対応するパスかどうかを判定する
	 * <pre>
	 *  Dialect=postgresqlの場合
	 *
	 *  - example/test.sql            : true
	 *  - postgresql/example/test.sql : true
	 *  - oracle/example/test.sql     : false
	 *  - example                     : true
	 *  - postgresql                  : true
	 *  - oracle                      : false
	 * </pre>
	 *
	 *
	 * @param path 検査対象のPath
	 * @return 妥当なPathの場合<code>true</code>
	 */
	private boolean validPath(final Path path) {
		var relativePath = relativePath(path);
		if (relativePath.equals(path)) {
			return true;
		}

		var d = relativePath.getName(0).toString().toLowerCase();
		// loadPathの直下が現在のdialect以外と一致する場合は無効なパスと判定する
		return !dialects.contains(d) || this.dialect.getDatabaseType().equals(d);

	}

	/**
	 * 指定されたPath配下のファイルを順次追跡し、sqlInfosに格納、または削除を行う。<br>
	 * また、監視対象指定があり、Pathがディレクトリの場合は、監視サービスに登録する。
	 *
	 * @param path 追跡を行うディレクトリ、またはファイルのPath
	 * @param watch 監視対象指定。<code>true</code>の場合監視対象
	 * @param remove 削除指定。<code>true</code>の場合、指定のPathを除外する。<code>false</code>の場合は格納する
	 */
	private void traverseFile(final Path path, final boolean watch, final boolean remove) {
		traceWith(LOG)
				.setMessage("traverseFile start. path : {}, watch : {}, remove : {}.")
				.addArgument(path)
				.addArgument(watch)
				.addArgument(remove)
				.log();
		if (Files.notExists(path)) {
			return;
		}
		if (Files.isDirectory(path)) {
			if (validPath(path)) {
				try (var ds = Files.newDirectoryStream(path)) {
					if (watch) {
						var key = path.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);
						watchDirs.put(key, path);
					}
					for (var child : ds) {
						traverseFile(child, watch, remove);
					}
				} catch (IOException ex) {
					throw new UroborosqlRuntimeException("I/O error occurred.", ex);
				}
			}
		} else if (path.toString().endsWith(fileExtension)) {
			var sqlName = getSqlName(path);
			this.sqlInfos.compute(sqlName, (k, v) -> v == null ? new SqlInfo(sqlName, path, loadPaths, dialect, charset)
					: v.computePath(path, remove));
		}
	}

	/**
	 * 指定されたjarのURL配下のファイルを順次追跡し、sqlInfosに格納を行う。<br>
	 *
	 * @param url 追跡を行うディレクトリ、またはファイルのURL
	 * @param loadPath SQLファイルをロードするパス
	 */
	@SuppressWarnings("resource")
	private void traverseJar(final URL url, final String loadPath) {
		traceWith(LOG)
				.setMessage("traverseJar start. url : {}, loadPath : {}.")
				.addArgument(url)
				.addArgument(loadPath)
				.log();
		FileSystem fs = null;
		try {
			var uri = url.toURI();
			try {
				fs = FileSystems.getFileSystem(uri);
			} catch (FileSystemNotFoundException ex) {
				fs = FileSystems.newFileSystem(uri, Map.of("create", "false"));
			}

			var conn = (JarURLConnection) url.openConnection();
			try (var jarFile = conn.getJarFile()) {
				var jarEntries = jarFile.entries();
				while (jarEntries.hasMoreElements()) {
					var jarEntry = jarEntries.nextElement();
					var name = jarEntry.getName();
					if (!jarEntry.isDirectory() && name.startsWith(loadPath) && name.endsWith(fileExtension)) {
						var path = fs.getPath(name);
						var sqlName = getSqlName(path);
						this.sqlInfos.compute(sqlName, (k, v) -> v == null
								? new SqlInfo(sqlName, path, loadPaths, dialect, charset)
								: v.computePath(path, false));
					}
				}
			}
		} catch (IOException | URISyntaxException ex) {
			throw new UroborosqlRuntimeException("I/O error occurred.", ex);
		} finally {
			if (fs != null) {
				try {
					fs.close();
				} catch (IOException ex) {
					throw new UroborosqlRuntimeException("I/O error occurred.", ex);
				}
			}
		}
	}

	/**
	 * SQLファイルの情報を保持するオブジェクト
	 */
	public static class SqlInfo implements ServiceLoggingSupport {
		/** キーとなるsqlName */
		private final String sqlName;
		/** 対象のDialect */
		private final Dialect dialect;
		/** Sqlファイルの文字コード */
		private final Charset charset;
		/** sqlNameに対応するPathのList. ソートされて優先度が高いものから順に並んでいる. 適用されるのは先頭のPathになる. */
		private final List<Path> pathList = new ArrayList<>();
		/** SQLファイルをロードするルートパスのリスト */
		private final List<Path> loadPaths;
		/** SQLファイルの内容. <code>null</code>の場合、getSqlBody()が呼び出された段階でロードして格納する. */
		private String sqlBody;
		/** 適用されたPathの最終更新日時。SQLファイルが更新されたかどうかの判定に利用する */
		private FileTime lastModified;

		/**
		 * コンストラクタ
		 * @param sqlName sqlName
		 * @param path path
		 * @param dialect dialect
		 * @param charset charset
		 */
		SqlInfo(final String sqlName,
				final Path path,
				final List<Path> loadPaths,
				final Dialect dialect,
				final Charset charset) {
			traceWith(LOG)
					.setMessage("SqlInfo - sqlName : {}, path : {}, dialect : {}, charset : {}.")
					.addArgument(sqlName)
					.addArgument(path)
					.addArgument(dialect)
					.addArgument(charset)
					.log();
			this.sqlName = sqlName;
			this.dialect = dialect;
			this.charset = charset;
			this.pathList.add(path);
			this.loadPaths = loadPaths;
			this.lastModified = getLastModifiedTime(path);
			this.sqlBody = null;
		}

		/**
		 * 指定されたPathの最終更新日時を取得する
		 * @param path 対象のPath
		 * @return 最終更新日時
		 */
		private FileTime getLastModifiedTime(final Path path) {
			if (SCHEME_FILE.equalsIgnoreCase(path.toUri().getScheme())) {
				try {
					return Files.getLastModifiedTime(path);
				} catch (IOException ex) {
					warnWith(LOG)
							.setMessage("Can't get lastModifiedTime. path:{}")
							.addArgument(path)
							.setCause(ex)
							.log();
				}
			}
			return FileTime.fromMillis(0L);
		}

		/**
		 * 指定されたPathがDialect指定のPathかどうかを判定する
		 * @param path 判定対象Path
		 * @return Dialect指定Pathの場合<code>true</code>
		 */
		private boolean hasDialect(final Path path) {
			for (var p : path) {
				if (this.dialect.getDatabaseType().equals(p.toString())) {
					return true;
				}
			}
			return false;
		}

		/**
		 * sqlName を取得します。
		 *
		 * @return sqlName
		 */
		public String getSqlName() {
			return sqlName;
		}

		/**
		 * 現在有効な path を取得します。
		 *
		 * @return path 現在有効なPath
		 */
		public Path getPath() {
			return pathList.get(0);
		}

		/**
		 * sql文字列を取得します。
		 * sqlBodyが<code>null</code>の場合、現在有効なPathからファイルの内容をロードし、sqlBodyに格納したうえ返却します。
		 *
		 * @return sqlBody sql文字列
		 */
		private String getSqlBody() {
			if (sqlBody == null) {
				var path = getPath();
				var scheme = path.toUri().getScheme();

				if (SCHEME_FILE.equalsIgnoreCase(scheme)) {
					// ファイルパスの場合
					if (Files.notExists(path)) {
						throw new UroborosqlRuntimeException("SQL template could not found.["
								+ path.toAbsolutePath().toString() + "]");
					}
					synchronized (sqlName) {
						try {
							var body = new String(Files.readAllBytes(path), charset);
							sqlBody = formatSqlBody(body);
							debugWith(LOG)
									.setMessage("Loaded SQL template.[{}]")
									.addArgument(path)
									.log();
						} catch (IOException ex) {
							throw new UroborosqlRuntimeException("Failed to load SQL template["
									+ path.toAbsolutePath().toString() + "].", ex);
						}
					}
				} else {
					// jarパスの場合
					var url = Thread.currentThread().getContextClassLoader().getResource(getResourcePath(path));
					if (url == null) {
						throw new UroborosqlRuntimeException("SQL template could not found.["
								+ path.toAbsolutePath().toString() + "]");
					}
					synchronized (sqlName) {
						try {
							var conn = url.openConnection();
							try (var reader = new BufferedReader(
									new InputStreamReader(conn.getInputStream(), charset))) {
								var body = reader.lines()
										.collect(Collectors.joining(System.lineSeparator()));
								sqlBody = formatSqlBody(body);
								debugWith(LOG)
										.setMessage("Loaded SQL template.[{}]")
										.addArgument(path)
										.log();
							}
						} catch (IOException ex) {
							throw new UroborosqlRuntimeException("Failed to load SQL template["
									+ path.toAbsolutePath().toString() + "].", ex);
						}
					}
				}
			}
			return sqlBody;
		}

		/**
		 * SQL文の不要な文字削除と末尾の改行文字付与を行う.
		 *
		 * @param sqlBody 元となるSQL文
		 * @return 整形後のSQL文
		 */
		protected String formatSqlBody(final String sqlBody) {
			var newBody = sqlBody.trim();
			if (newBody.endsWith("/") && !newBody.endsWith("*/")) {
				newBody = ObjectUtils.removeEnd(newBody, "/");
			} else {
				newBody = newBody + System.lineSeparator();
			}
			return newBody;
		}

		/**
		 * 同じSqlNameになるPathの優先度判定を行い、優先度が高いPathが指定された場合保持しているPathの置き換えを行う
		 *
		 * @param newPath 判定用Path
		 * @param remove 指定した判定用Pathを削除する場合に<code>true</code>を指定
		 *
		 * @return 判定後のSqlInfo
		 */
		private SqlInfo computePath(final Path newPath, final boolean remove) {
			synchronized (sqlName) {
				// 変更前の有効Pathを保持しておく
				var oldPath = getPath();

				// 引数で渡された判定用PathをpathListへ追加、またはpathListから削除する
				if (!pathList.contains(newPath)) {
					if (!remove) {
						pathList.add(newPath);
					}
				} else {
					if (remove) {
						pathList.remove(newPath);
						if (pathList.isEmpty()) {
							// pathListが空になった場合はこのSqlInfoをsqlInfosから除外するためにnullを返す
							return null;
						}
					}
				}

				if (pathList.size() > 1) {
					// 優先度が高いPathが先頭に来るようにソートを行う
					// 1. Dialect付Pathを優先
					// 2. file を jar よりも優先
					// 3. Path同士のcompare
					pathList.sort((p1, p2) -> {
						if (p1 == null && p2 == null) {
							return 0;
						} else if (p1 != null && p2 == null) {
							return -1;
						} else if (p1 == null && p2 != null) {
							return 1;
						}

						// DialectPathの比較
						var p1HasDialect = hasDialect(p1);
						var p2HasDialect = hasDialect(p2);

						if (p1HasDialect && !p2HasDialect) {
							return -1;
						} else if (!p1HasDialect && p2HasDialect) {
							return 1;
						}

						// schemeの比較
						var p1Scheme = p1.toUri().getScheme();
						var p2Scheme = p2.toUri().getScheme();

						if (!p1Scheme.equals(p2Scheme)) {
							if (SCHEME_FILE.equals(p1Scheme)) {
								return -1;
							} else {
								return 1;
							}
						}

						// LoadPathsの並び順にソート
						var p1Pos = 0;
						var p2Pos = 0;
						for (var pos = 0; pos < this.loadPaths.size(); pos++) {
							var loadPath = this.loadPaths.get(pos);
							var loadPathSize = loadPath.getNameCount();

							for (var i = 0; i < p1.getNameCount() - loadPathSize; i++) {
								var p1SubPath = p1.subpath(i, i + loadPathSize);
								if (p1SubPath.equals(loadPath)) {
									p1Pos = pos + 1;
									break;
								}
							}
							for (var i = 0; i < p2.getNameCount() - loadPathSize; i++) {
								var p2SubPath = p2.subpath(i, i + loadPathSize);
								if (p2SubPath.equals(loadPath)) {
									p2Pos = pos + 1;
									break;
								}
							}
							if (p1Pos > 0 && p2Pos > 0) {
								break;
							}
						}
						if (p1Pos != p2Pos) {
							return p1Pos - p2Pos;
						}

						return p1.compareTo(p2);
					});
				}

				var replaceFlag = false;
				// ソートによる再計算後の有効Pathを取得する
				var currentPath = getPath();
				var currentTimeStamp = getLastModifiedTime(currentPath);
				if (!oldPath.equals(currentPath)) {
					replaceFlag = true;
					debugWith(LOG)
							.setMessage("sql file switched. sqlName={}, oldPath={}, newPath={}, lastModified={}")
							.addArgument(sqlName)
							.addArgument(oldPath)
							.addArgument(currentPath)
							.addArgument(currentTimeStamp)
							.log();
				} else {
					if (!this.lastModified.equals(currentTimeStamp)) {
						replaceFlag = true;
						debugWith(LOG)
								.setMessage("sql file changed. sqlName={}, path={}, lastModified={}")
								.addArgument(sqlName)
								.addArgument(currentPath)
								.addArgument(currentTimeStamp)
								.log();
					}
				}

				if (replaceFlag) {
					this.lastModified = currentTimeStamp;
					this.sqlBody = null;
				}
				return this;
			}
		}

		/**
		 * クラスローダーから取得する際のリソースパス（loadPathで始まるパス）を取得する.<br>
		 * loadPathと一致する部分がなかった場合は、引数のpathの値をそのまま返却する
		 *
		 * @param path 計算対象のパス
		 * @return クラスローダーから取得する際のリソースパス
		 */
		private String getResourcePath(final Path path) {
			var pathSize = path.getNameCount();

			for (var loadPath : this.loadPaths) {
				var loadPathSize = loadPath.getNameCount();

				for (var i = 0; i < pathSize - loadPathSize; i++) {
					var subPath = path.subpath(i, i + loadPathSize);
					if (loadPath.equals(subPath)) {
						return path.subpath(i, pathSize).toString();
					}
				}
			}
			return path.toString(); // loadPathと一致しなかった場合はパスをそのまま返却する。
		}
	}
}
