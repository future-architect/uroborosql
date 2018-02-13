package jp.co.future.uroborosql.store;

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
import java.nio.file.attribute.FileTime;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class NioSqlManagerImpl implements SqlManager {
	private static final String SCHEME_JAR = "jar";

	private static final String SCHEME_FILE = "file";

	private static final String PATH_SEPARATOR = "/";

	/** ロガー */
	private static final Logger log = LoggerFactory.getLogger(NioSqlManagerImpl.class);

	private static Set<String> dialects = StreamSupport
			.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
			.map(d -> d.getDialectName()).collect(Collectors.toSet());

	/** SQLファイルをロードするルートパス */
	private String loadPath = "sql";

	/**
	 * SQLファイル拡張子
	 */
	private String fileExtension = ".sql";

	/**
	 * SQLファイルエンコーディング
	 */
	private final Charset charset;

	private Dialect dialect;

	private ConcurrentHashMap<String, SqlInfo> sqlInfos = new ConcurrentHashMap<>();

	public NioSqlManagerImpl() {
		this("sql");
	}

	public NioSqlManagerImpl(final String loadPath) {
		this(loadPath, ".sql");
	}

	public NioSqlManagerImpl(final String loadPath, final String fileExtension) {
		this(loadPath, fileExtension, Charset.forName(System.getProperty("file.encoding")));
	}

	public NioSqlManagerImpl(final String loadPath, final String fileExtension, final Charset charset) {
		this.loadPath = loadPath;
		this.fileExtension = fileExtension;
		if (charset != null) {
			this.charset = charset;
		} else {
			this.charset = Charset.forName(System.getProperty("file.encoding"));
		}
	}

	private ConcurrentHashMap<String, SqlInfo> getSqlInfos() {
		ConcurrentHashMap<String, SqlInfo> infos = new ConcurrentHashMap<>();

		try {
			Enumeration<URL> root = this.getClass().getClassLoader().getResources(loadPath);

			while (root.hasMoreElements()) {
				URI uri = root.nextElement().toURI();
				String scheme = uri.getScheme();
				if (SCHEME_FILE.equals(scheme)) {
					traverse(infos, Paths.get(uri));
				} else if (SCHEME_JAR.equals(scheme)) {
					FileSystem fs = null;
					try {
						fs = FileSystems.getFileSystem(uri);
					} catch (FileSystemNotFoundException ex) {
						Map<String, String> env = new HashMap<>();
						env.put("create", "false");
						fs = FileSystems.newFileSystem(uri, env);
					}
					traverse(infos, fs.getPath(loadPath));
				}
			}

		} catch (IOException | URISyntaxException e) {
			e.printStackTrace();
		}

		return infos;
	}

	@Override
	public boolean existSql(final String sqlName) {
		return sqlInfos.containsKey(sqlName);
	}

	@Override
	public String getSql(final String sqlName) {
		if (existSql(sqlName)) {
			return sqlInfos.get(sqlName).getSqlBody();
		} else {
			throw new UroborosqlRuntimeException("sql file not found. sqlName : " + sqlName);
		}
	}

	/**
	 * SqlNameを与えられたPathから生成する<br>
	 *
	 * <pre>
	 * SqlNameは以下のルールで生成する
	 * 1. loadPathで指定されたフォルダの下のフォルダ名とファイル名を"/"でつなげた文字列とする
	 * 2. loadPathの直下にdialectと一致するフォルダがある場合は、dialectフォルダの下のフォルダとファイル名を"/"でつなげた文字列とする
	 *
	 * ex)
	 *
	 *  sql/
	 *    example/
	 *      test1.sql
	 *      test2.sql
	 *    oracle/
	 *      example/
	 *        test1.sql
	 *    postgresql/
	 *      example/
	 *        test2.sql
	 *
	 *   上記のフォルダ構成で
	 *   - loadPath=sql, dialect=oracleの場合は以下のSqlNameが生成される
	 *     example/test1.sql ( 実際はoracle/example/test1.sql )
	 *     example/test2.sql
	 *   - loadPath=sql, dialect=postgresqlの場合は以下のSqlNameが生成される
	 *     example/test1.sql
	 *     example/test2.sql ( 実際はpostgresql/example/test2.sql )
	 *
	 * </pre>
	 *
	 * @param path Path ファイルパス
	 * @return SqlName SqlName
	 */
	private String getSqlName(final Path path) {
		StringBuilder builder = new StringBuilder();

		boolean startFlag = false;
		boolean dialectFlag = false;
		for (Path part : path) {
			if (startFlag) {
				String s = part.toString();
				if (dialectFlag) {
					// loadPathの直下がdialectと一致する場合はその下のフォルダから名前を付ける
					dialectFlag = false;
					if (dialects.contains(s.toLowerCase())) {
						continue;
					}
				}
				builder.append(s).append(PATH_SEPARATOR);
			} else if (part.toString().equals(loadPath)) {
				startFlag = true;
				dialectFlag = true;
			}
		}

		return builder.substring(0, builder.length() - (fileExtension.length() + 1));
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
		int count = 0;
		boolean matchFlag = false;
		for (Path p : path) {
			count++;
			if (p.toString().equals(loadPath)) {
				matchFlag = true;
				break;
			}
		}

		// loadPath と一致しない場合は有効と判定
		if (!matchFlag) {
			return true;
		}

		if (count >= path.getNameCount()) {
			return true;
		}

		String d = path.getName(count).toString().toLowerCase();
		// loadPathの直下が現在のdialect以外と一致する場合は無効なパスと判定する
		if (dialects.contains(d)) {
			return this.dialect.getDialectName().equals(d);
		} else {
			return true;
		}

	}

	private void traverse(final Map<String, SqlInfo> sqlInfos, final Path path) {
		if (Files.notExists(path)) {
			return;
		}
		if (Files.isDirectory(path)) {
			if (validPath(path)) {
				try (DirectoryStream<Path> ds = Files.newDirectoryStream(path)) {
					for (Path child : ds) {
						traverse(sqlInfos, child);
					}
				} catch (IOException ex) {
					throw new UroborosqlRuntimeException("I/O error occured.", ex);
				}
			}
		} else if (path.toString().endsWith(fileExtension)) {
			String sqlName = getSqlName(path);
			sqlInfos.compute(sqlName,
					(k, v) -> (v == null) ? new SqlInfo(sqlName, path, dialect, charset) : v.computePath(path));
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
	 * dialect を取得します。
	 *
	 * @return dialect
	 */
	@Override
	public Dialect getDialect() {
		return dialect;
	}

	/**
	 * dialect を設定します。
	 *
	 * @param dialect dialect
	 */
	@Override
	public void setDialect(final Dialect dialect) {
		this.dialect = dialect;
	}

	@Override
	public void initialize() {
		this.sqlInfos = getSqlInfos();
	}

	@Override
	public List<String> getSqlPathList() {
		return Collections.list(this.sqlInfos.keys());
	}

	@Override
	public SqlLoader getSqlLoader() {
		throw new UnsupportedOperationException();
	}

	@Override
	public void setSqlLoader(final SqlLoader sqlLoader) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean isCache() {
		throw new UnsupportedOperationException();
	}

	@Override
	public void setCache(final boolean cache) {
		throw new UnsupportedOperationException();
	}

	/**
	 * SQLファイルの情報を保持するオブジェクト
	 *
	 * @author H.Sugimoto
	 *
	 */
	public static class SqlInfo {

		private final String sqlName;
		private Path path;
		private String scheme;
		private final Dialect dialect;
		private final Charset charset;
		private String sqlBody;
		private FileTime lastModified;

		SqlInfo(final String sqlName, final Path path, final Dialect dialect, final Charset charset) {
			super();
			this.sqlName = sqlName;
			this.path = path;
			this.scheme = path.toUri().getScheme();
			this.dialect = dialect;
			this.charset = charset;
			this.sqlBody = null;
			this.lastModified = getLastModifiedTime(path);
		}

		private FileTime getLastModifiedTime(final Path path) {
			try {
				return Files.getLastModifiedTime(path);
			} catch (IOException e) {
				e.printStackTrace();
			}
			return FileTime.fromMillis(0L);
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
		 * path を取得します。
		 *
		 * @return path
		 */
		public Path getPath() {
			return path;
		}

		/**
		 * sqlBody を取得します。
		 *
		 * @return sqlBody
		 */
		public String getSqlBody() {
			if (sqlBody == null) {
				if (Files.notExists(path)) {
					throw new UroborosqlRuntimeException("SQL template could not found.["
							+ path.toAbsolutePath().toString() + "]");
				}

				synchronized (sqlName) {
					try {
						sqlBody = new String(Files.readAllBytes(path), charset).trim();
						if (sqlBody.endsWith(PATH_SEPARATOR) && !sqlBody.endsWith("*/")) {
							sqlBody = StringUtils.stripEnd(sqlBody, PATH_SEPARATOR);
						} else {
							sqlBody = sqlBody + System.lineSeparator();
						}
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
		 * lastModified を取得します。
		 *
		 * @return lastModified
		 */
		public FileTime getLastModified() {
			return lastModified;
		}

		/**
		 * 同じSqlNameになるPathの優先度判定を行い、優先度が高いPathが指定された場合保持しているPathの置き換えを行う
		 *
		 * @param newPath 判定用Path
		 * @return 判定後のSqlInfo
		 */
		public SqlInfo computePath(final Path newPath) {
			boolean replaceFlag = false;
			String newScheme = newPath.toUri().getScheme();
			if (scheme.equals(newScheme)) {
				for (Path p : newPath) {
					if (p.toString().equals(this.dialect.getDialectName())) {
						replaceFlag = true;
						break;
					}
				}
			} else if (SCHEME_JAR.equals(scheme) && SCHEME_FILE.equals(newScheme)) {
				replaceFlag = true;
			}

			if (replaceFlag) {
				synchronized (sqlName) {
					this.path = newPath;
					this.scheme = newScheme;
					this.lastModified = getLastModifiedTime(newPath);
					this.sqlBody = null;
				}
			}
			return this;
		}

	}
}
