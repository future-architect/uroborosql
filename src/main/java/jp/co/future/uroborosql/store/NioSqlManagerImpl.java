/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import static java.nio.file.StandardWatchEventKinds.*;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.DirectoryStream;
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
import java.util.Enumeration;
import java.util.HashMap;
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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.utils.StringUtils;

public class NioSqlManagerImpl implements SqlManager {
	/** ロガー */
	private static final Logger log = LoggerFactory.getLogger(NioSqlManagerImpl.class);

	/** zip, jar内のファイルのscheme */
	private static final String SCHEME_JAR = "jar";
	/** ファイルシステム上のファイルのscheme */
	private static final String SCHEME_FILE = "file";

	/** SQLファイルロードのデフォルトルートパス */
	private static final String DEFAULT_LOAD_PATH = "sql";

	/** 有効なDialectのSet */
	private static final Set<String> dialects = StreamSupport
			.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
			.map(Dialect::getDatabaseType).collect(Collectors.toSet());

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
	private final ConcurrentHashMap<String, SqlInfo> sqlInfos = new ConcurrentHashMap<>();

	/** WatchKeyに対するディレクトリPathを取得するためのMap */
	private final ConcurrentHashMap<WatchKey, Path> watchDirs = new ConcurrentHashMap<>();

	/**
	 * コンストラクタ
	 */
	public NioSqlManagerImpl() {
		this(DEFAULT_LOAD_PATH);
	}

	/**
	 * コンストラクタ
	 *
	 * @param detectChanges SQLファイルの変更を検知するかどうか
	 */
	public NioSqlManagerImpl(final boolean detectChanges) {
		this(DEFAULT_LOAD_PATH, null, null, detectChanges);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 */
	public NioSqlManagerImpl(final String loadPath) {
		this(loadPath, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param fileExtension SQLファイル拡張子
	 */
	public NioSqlManagerImpl(final String loadPath, final String fileExtension) {
		this(loadPath, fileExtension, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 */
	public NioSqlManagerImpl(final String loadPath, final String fileExtension, final Charset charset) {
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
	public NioSqlManagerImpl(final String loadPath, final String fileExtension, final Charset charset,
			final boolean detectChanges) {
		this(Arrays.asList(loadPath), fileExtension, charset, detectChanges);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLファイルをロードするルートパスのリスト
	 */
	public NioSqlManagerImpl(final List<String> loadPaths) {
		this(loadPaths, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLファイルをロードするルートパスのリスト
	 * @param fileExtension SQLファイル拡張子
	 */
	public NioSqlManagerImpl(final List<String> loadPaths, final String fileExtension) {
		this(loadPaths, fileExtension, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLファイルをロードするルートパスのリスト
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 */
	public NioSqlManagerImpl(final List<String> loadPaths, final String fileExtension, final Charset charset) {
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
	public NioSqlManagerImpl(final List<String> loadPaths, final String fileExtension, final Charset charset,
			final boolean detectChanges) {
		this.loadPaths = new ArrayList<>();
		for (String loadPath : loadPaths) {
			if (loadPath == null) {
				throw new IllegalArgumentException("loadPath is required.");
			}
			this.loadPaths.add(Paths.get(loadPath));
		}
		this.loadPathPartsList = new ArrayList<>();
		for (Path loadPath : this.loadPaths) {
			List<String> pathList = new ArrayList<>();
			for (Path part : loadPath) {
				pathList.add(part.toString());
			}
			this.loadPathPartsList.add(pathList.toArray(new String[pathList.size()]));
		}
		this.fileExtension = fileExtension != null ? fileExtension : ".sql";
		this.charset = charset != null ? charset : Charset.forName(System.getProperty("file.encoding"));
		this.detectChanges = detectChanges;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#initialize()
	 */
	@Override
	public void initialize() {
		if (detectChanges) {
			try {
				watcher = FileSystems.getDefault().newWatchService();
			} catch (IOException e) {
				log.error("Can't start watcher service.", e);
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
	 * @see jp.co.future.uroborosql.store.SqlManager#shutdown()
	 */
	@Override
	public void shutdown() {
		if (detectChanges) {
			es.shutdown();
			try {
				if (!es.awaitTermination(1000, TimeUnit.MILLISECONDS)) {
					es.shutdownNow();
				}
			} catch (InterruptedException e) {
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
				log.debug("WatchService catched InterruptedException.");
				break;
			} catch (Throwable ex) {
				log.error("Unexpected exception occurred.", ex);
				break;
			}

			for (WatchEvent<?> event : key.pollEvents()) {
				WatchEvent.Kind<?> kind = event.kind();

				if (kind == OVERFLOW) {
					continue;
				}

				//ファイル名はイベントのコンテキストです。
				@SuppressWarnings("unchecked")
				WatchEvent<Path> evt = (WatchEvent<Path>) event;
				Path dir = watchDirs.get(key);
				Path path = dir.resolve(evt.context());

				log.trace("file changed.({}). path={}", kind.name(), path);
				boolean isSqlFile = path.toString().endsWith(fileExtension);
				if (Files.isDirectory(path) || !isSqlFile) {
					// ENTRY_DELETEの時はFiles.isDirectory()がfalseになるので拡張子での判定も行う
					if (kind == ENTRY_CREATE) {
						traverse(path, true, false);
					} else if (kind == ENTRY_DELETE) {
						key.cancel();
						watchDirs.remove(key);
						continue;
					}
				} else if (isSqlFile) {
					if (kind == ENTRY_CREATE) {
						traverse(path, true, false);
					} else if (kind == ENTRY_MODIFY || kind == ENTRY_DELETE) {
						String sqlName = getSqlName(path);
						sqlInfos.computeIfPresent(sqlName, (k, v) -> {
							return v.computePath(path, kind == ENTRY_DELETE);
						});
					}
				}
			}
			key.reset();
		}
		try {
			watcher.close();
		} catch (IOException e) {
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
	 * @see jp.co.future.uroborosql.store.SqlManager#getDialect()
	 */
	@Override
	public Dialect getDialect() {
		return dialect;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#setDialect(jp.co.future.uroborosql.dialect.Dialect)
	 */
	@Override
	public void setDialect(final Dialect dialect) {
		this.dialect = dialect;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#getSqlPathList()
	 */
	@Override
	public List<String> getSqlPathList() {
		List<String> list = Collections.list(this.sqlInfos.keys());
		Collections.sort(list);
		return list;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#getSqlLoader()
	 */
	@Override
	public SqlLoader getSqlLoader() {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#setSqlLoader(jp.co.future.uroborosql.store.SqlLoader)
	 */
	@Override
	public void setSqlLoader(final SqlLoader sqlLoader) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#isCache()
	 */
	@Override
	public boolean isCache() {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#setCache(boolean)
	 */
	@Override
	public void setCache(final boolean cache) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#existSql(java.lang.String)
	 */
	@Override
	public boolean existSql(final String sqlName) {
		return sqlInfos.containsKey(sqlName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#getSql(java.lang.String)
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
	private void generateSqlInfos() {
		try {
			for (Path loadPath : this.loadPaths) {
				String loadPathSlash = loadPath.toString().replaceAll("\\\\", "/");
				Enumeration<URL> root = Thread.currentThread().getContextClassLoader()
						.getResources(loadPathSlash);

				while (root.hasMoreElements()) {
					URI uri = root.nextElement().toURI();
					String scheme = uri.getScheme();
					if (SCHEME_FILE.equals(scheme)) {
						traverse(Paths.get(uri), detectChanges && true, false);
					} else if (SCHEME_JAR.equals(scheme)) {
						FileSystem fs = null;
						try {
							fs = FileSystems.getFileSystem(uri);
						} catch (FileSystemNotFoundException ex) {
							Map<String, String> env = new HashMap<>();
							env.put("create", "false");
							fs = FileSystems.newFileSystem(uri, env);
						}
						traverse(fs.getPath(loadPathSlash), false, false);
					}
				}
			}
		} catch (IOException | URISyntaxException e) {
			log.error("Can't load sql files.", e);
		}
	}

	/**
	 * SqlNameを与えられたPathから生成する<br>
	 *
	 * <pre>
	 * SqlNameは以下のルールで生成する
	 * 1. loadPathで指定されたフォルダの下のフォルダ名とファイル名を"/"でつなげた文字列とする
	 * 2. loadPathの直下にdialectと一致するフォルダがある場合は、dialectフォルダの下のフォルダとファイル名を"/"でつなげた文字列とする
	 * 3. loadPathが複数指定されていて、同名のsqlNameがある場合、loadPathの並び順で先になるものを優先する
	 *
	 * ex)
	 *
	 *  sql/
	 *    example/
	 *      test1.sql
	 *      test2.sql
	 *      test3.sql
	 *    oracle/
	 *      example/
	 *        test1.sql
	 *    postgresql/
	 *      example/
	 *        test2.sql
	 *  secondary_sql/
	 *    example/
	 *      test3.sql
	 *      test4.sql
	 *
	 *   上記のフォルダ構成で
	 *   - loadPath=sql, dialect=oracleの場合は以下のSqlNameが生成される
	 *     example/test1 ( 実際はoracle/example/test1 )
	 *     example/test2
	 *   - loadPath=sql, dialect=postgresqlの場合は以下のSqlNameが生成される
	 *     example/test1
	 *     example/test2 ( 実際はpostgresql/example/test2 )
	 *   - loadPath=[sql, secondary_sql], dialect=postgresqlの場合は以下のSqlNameが生成される
	 *     example/test1
	 *     example/test2 ( 実際はpostgresql/example/test2 )
	 *     example/test3
	 *     example/test4
	 *   - loadPath=[secondary_sql, sql], dialect=postgresqlの場合は以下のSqlNameが生成される
	 *     example/test1
	 *     example/test2 ( 実際はpostgresql/example/test2 )
	 *     example/test3 ( 実際はsecondary_sql/example/test3 )
	 *     example/test4
	 *
	 * </pre>
	 *
	 * @param path Path ファイルパス
	 * @return SqlName SqlName
	 */
	private String getSqlName(final Path path) {
		StringBuilder builder = new StringBuilder();

		boolean dialectFlag = true;
		for (Path part : relativePath(path)) {
			String s = part.toString();
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
	 * loadPathからの相対パスを取得する.
	 * loadPathと一致する部分がなかった場合は、引数のpathの値をそのまま返却する
	 *
	 * @param path 相対パスを取得する元のパス
	 * @return 相対パス
	 */
	private Path relativePath(final Path path) {
		List<Path> pathList = new ArrayList<>();
		for (Path part : path) {
			pathList.add(part);
		}

		for (String[] loadPathParts : this.loadPathPartsList) {
			int loadPathSize = loadPathParts.length;

			// loadPathのフォルダの並びと一致する場所を特定し、その下を相対パスとして返却する
			for (int i = 0; i < pathList.size() - loadPathSize; i++) {
				String[] paths = pathList.subList(i, i + loadPathSize).stream()
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
		Path relativePath = relativePath(path);
		if (relativePath.equals(path)) {
			return true;
		}

		String d = relativePath.getName(0).toString().toLowerCase();
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
	private void traverse(final Path path, final boolean watch, final boolean remove) {
		log.debug("traverse start. path : {}, watch : {}, remove : {}", path, watch, remove);
		if (Files.notExists(path)) {
			return;
		}
		if (Files.isDirectory(path)) {
			if (validPath(path)) {
				try (DirectoryStream<Path> ds = Files.newDirectoryStream(path)) {
					if (watch) {
						WatchKey key = path.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);
						watchDirs.put(key, path);
					}
					for (Path child : ds) {
						traverse(child, watch, remove);
					}
				} catch (IOException ex) {
					throw new UroborosqlRuntimeException("I/O error occurred.", ex);
				}
			}
		} else if (path.toString().endsWith(fileExtension)) {
			String sqlName = getSqlName(path);
			this.sqlInfos.compute(sqlName,
					(k, v) -> v == null ? new SqlInfo(sqlName, path, loadPaths, dialect, charset)
							: v.computePath(path, remove));
		}
	}

	/**
	 * SQLファイルの情報を保持するオブジェクト
	 */
	public static class SqlInfo {

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
		SqlInfo(final String sqlName, final Path path, final List<Path> loadPaths, final Dialect dialect,
				final Charset charset) {
			super();
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
		private static FileTime getLastModifiedTime(final Path path) {
			try {
				return Files.getLastModifiedTime(path);
			} catch (IOException e) {
				log.error("Can't get lastModifiedTime. path:" + path, e);
			}
			return FileTime.fromMillis(0L);
		}

		/**
		 * 指定されたPathがDialect指定のPathかどうかを判定する
		 * @param path 判定対象Path
		 * @return Dialect指定Pathの場合<code>true</code>
		 */
		private boolean hasDialect(final Path path) {
			for (Path p : path) {
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
				Path path = getPath();
				if (Files.notExists(path)) {
					throw new UroborosqlRuntimeException("SQL template could not found.["
							+ path.toAbsolutePath().toString() + "]");
				}

				synchronized (sqlName) {
					try {
						String body = new String(Files.readAllBytes(path), charset).trim();
						if (body.endsWith("/") && !body.endsWith("*/")) {
							body = StringUtils.removeEnd(body, "/");
						} else {
							body = body + System.lineSeparator();
						}
						sqlBody = body;
						log.debug("Loaded SQL template.[{}]", path);
					} catch (IOException e) {
						throw new UroborosqlRuntimeException("Failed to load SQL template["
								+ path.toAbsolutePath().toString() + "].", e);
					}
				}
			}

			return sqlBody;
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
				Path oldPath = getPath();

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
					// 2. file schemeをjar schemeよりも優先
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
						boolean p1HasDialect = hasDialect(p1);
						boolean p2HasDialect = hasDialect(p2);

						if (p1HasDialect && !p2HasDialect) {
							return -1;
						} else if (!p1HasDialect && p2HasDialect) {
							return 1;
						}

						// schemeの比較
						String p1Scheme = p1.toUri().getScheme();
						String p2Scheme = p2.toUri().getScheme();

						if (!p1Scheme.equals(p2Scheme)) {
							if (p1Scheme.equals(SCHEME_FILE)) {
								return -1;
							} else {
								return 1;
							}
						}

						// LoadPathsの並び順にソート
						int p1Pos = 0;
						int p2Pos = 0;
						for (int pos = 0; pos < this.loadPaths.size(); pos++) {
							Path loadPath = this.loadPaths.get(pos);
							int loadPathSize = loadPath.getNameCount();

							for (int i = 0; i < p1.getNameCount() - loadPathSize; i++) {
								Path p1SubPath = p1.subpath(i, i + loadPathSize);
								if (p1SubPath.equals(loadPath)) {
									p1Pos = pos + 1;
									break;
								}
							}
							for (int i = 0; i < p2.getNameCount() - loadPathSize; i++) {
								Path p2SubPath = p2.subpath(i, i + loadPathSize);
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

				boolean replaceFlag = false;
				// ソートによる再計算後の有効Pathを取得する
				Path currentPath = getPath();
				FileTime currentTimeStamp = getLastModifiedTime(currentPath);
				if (!oldPath.equals(currentPath)) {
					replaceFlag = true;
					log.trace("sql file switched. sqlName={}, oldPath={}, newPath={}, lastModified={}", sqlName,
							oldPath, currentPath, currentTimeStamp.toString());
				} else {
					if (!this.lastModified.equals(currentTimeStamp)) {
						replaceFlag = true;
						log.trace("sql file changed. sqlName={}, path={}, lastModified={}", sqlName, currentPath,
								currentTimeStamp.toString());
					}
				}

				if (replaceFlag) {
					this.lastModified = currentTimeStamp;
					this.sqlBody = null;
				}
				return this;
			}
		}
	}
}
