/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.ServiceLoader;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.utils.ObjectUtils;

/**
 * SQLリソース管理実装クラス
 *
 * @author H.Sugimoto
 */
public class SqlResourceManagerImpl implements SqlResourceManager {
	/** ファイルシステム上のファイルのscheme */
	private static final String SCHEME_FILE = "file";

	/** 有効なDialectのSet */
	private static final Set<String> dialects = StreamSupport
			.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
			.map(dialect -> dialect.getDatabaseType().toLowerCase())
			.collect(Collectors.toSet());

	/** CharBufferのキャパシティ. */
	private static final int BUFFER_CAPACITY = 8 * 1024;

	/** SQLファイルをロードするルートパスのリスト */
	private final List<Path> loadPaths;

	/** SQLファイルをロードするルートパスの各階層を保持する配列（ルートパス特定用） */
	private final List<String[]> loadPathPartsList;

	/** SQLファイル拡張子 */
	private final String fileExtension;

	/** SQLファイルエンコーディング */
	private final Charset charset;

	/** Dialect */
	private Dialect dialect;

	/** sqlNameとそれに対するSqlInfoの紐付きを持つMap */
	protected final ConcurrentHashMap<String, SqlInfo> sqlInfos = new ConcurrentHashMap<>();

	/** クラスローダー */
	private final ClassLoader classLoader;

	/**
	 * コンストラクタ
	 */
	public SqlResourceManagerImpl() {
		this(DEFAULT_LOAD_PATH);
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
		this(List.of(loadPath), fileExtension, charset, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 * @param classLoader SQLをロードするために利用するクラスローダー
	 */
	public SqlResourceManagerImpl(final String loadPath, final String fileExtension, final Charset charset,
			final ClassLoader classLoader) {
		this(List.of(loadPath), fileExtension, charset, classLoader);
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
		this(loadPaths, fileExtension, null, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLファイルをロードするルートパスのリスト
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 * @param classLoader SQLをロードするために利用するクラスローダー
	 *
	 * @throws IllegalArgumentException loadPathsに<code>null</code>が含まれる場合
	 */
	public SqlResourceManagerImpl(final List<String> loadPaths, final String fileExtension, final Charset charset,
			final ClassLoader classLoader) {
		this.loadPaths = loadPaths.stream()
				.filter(Objects::nonNull)
				.map(Paths::get)
				.collect(Collectors.toList());
		this.loadPathPartsList = this.loadPaths.stream()
				.map(path -> StreamSupport.stream(path.spliterator(), false)
						.map(Path::toString)
						.toArray(String[]::new))
				.collect(Collectors.toList());
		this.fileExtension = fileExtension != null ? fileExtension : ".sql";
		this.charset = charset != null ? charset : Charset.forName(Charset.defaultCharset().displayName());
		this.classLoader = classLoader != null ? classLoader : Thread.currentThread().getContextClassLoader();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#initialize()
	 */
	@Override
	public void initialize() {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#shutdown()
	 */
	@Override
	public void shutdown() {
		this.sqlInfos.clear();
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
			try {
				if (generateSqlInfo(sqlName)) {
					return sqlInfos.get(sqlName).getSqlBody();
				} else {
					throw new UroborosqlRuntimeException("sql file not found. sqlName : " + sqlName);
				}
			} catch (IOException ex) {
				throw new UroborosqlRuntimeException("I/O error occurred. sqName : " + sqlName, ex);
			}
		}
	}

	/**
	 * sqlNameに対するSqlInfoを生成する.
	 *
	 * @param sqlName SQL名
	 * @return 生成に成功した場合<code>true</code>
	 * @throws IOException SQLの読み込みに失敗した場合
	 */
	protected boolean generateSqlInfo(final String sqlName) throws IOException {
		for (var loadPath : loadPaths) {
			var path = getDialectSqlPath(loadPath, sqlName);
			var url = getResource(path);
			if (url == null) {
				path = getDefaultSqlPath(loadPath, sqlName);
				url = getResource(path);
			}
			if (url != null) {
				try {
					var scheme = url.toURI().getScheme().toLowerCase();
					var sqlBody = loadSql(url);
					var sqlInfo = new SqlInfo(path, loadPath, scheme, sqlBody);
					this.sqlInfos.put(sqlName, sqlInfo);
					traceWith(LOG)
							.setMessage("SqlInfo - sqlName : {}, path : {}, rootPath : {}, scheme : {}, sqlBody : {}.")
							.addArgument(sqlName)
							.addArgument(sqlInfo.getPath())
							.addArgument(sqlInfo.getRootPath())
							.addArgument(sqlInfo.getScheme())
							.addArgument(sqlInfo.getSqlBody())
							.log();
					return true;
				} catch (URISyntaxException ex) {
					throw new IOException(ex);
				}
			}
		}
		return false;
	}

	/**
	 * sqlNameに対するDialectフォルダを含まないファイルのパスを取得する
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param sqlName SQL名
	 * @return ルートパスからDialectフォルダを含まないSQLのパス
	 */
	protected Path getDefaultSqlPath(final Path loadPath, final String sqlName) {
		return loadPath.resolve(sqlName + fileExtension);
	}

	/**
	 * sqlNameに対するDialectフォルダを含むファイルのパスを取得する
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param sqlName SQL名
	 * @return ルートパスからDialectフォルダを含むSQLのパス
	 */
	protected Path getDialectSqlPath(final Path loadPath, final String sqlName) {
		return loadPath.resolve(dialect.getDatabaseType().toLowerCase()).resolve(sqlName + fileExtension);
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
			try {
				if (generateSqlInfo(sqlName)) {
					return sqlInfos.get(sqlName).getPath();
				} else {
					throw new UroborosqlRuntimeException("sql file not found. sqlName : " + sqlName);
				}
			} catch (IOException ex) {
				throw new UroborosqlRuntimeException("I/O error occurred. sqName : " + sqlName, ex);
			}
		}
	}

	/**
	 * クラスローダーからリソースのURLを取得する
	 *
	 * @param path リソースのパス
	 * @return リソースのURL. リソースが存在しない場合は<code>null</code>
	 */
	protected URL getResource(final Path path) throws IOException {
		var resources = Collections.list(classLoader.getResources(path.toString().replace('\\', '/')));
		if (resources.isEmpty()) {
			return null;
		}
		try {
			for (URL resource : resources) {
				if (SCHEME_FILE.equalsIgnoreCase(resource.toURI().getScheme())) {
					return resource;
				}
			}
		} catch (URISyntaxException ex) {
			throw new IOException(ex);
		}
		return resources.get(0);
	}

	/**
	 * 指定したURLのSQLファイルのSQL本文をロードする.
	 *
	 * @param url SQLファイルのURL
	 * @return SQL本文
	 * @throws IOException ファイルの読み込みに失敗した場合
	 */
	protected String loadSql(final URL url) throws IOException {
		try (var reader = new BufferedReader(new InputStreamReader(url.openStream(), getCharset()))) {
			var builder = new StringBuilder();
			var charBuffer = CharBuffer.allocate(BUFFER_CAPACITY);
			var numCharsRead = 0;
			while ((numCharsRead = reader.read(charBuffer)) != -1) {
				builder.append(charBuffer.array(), 0, numCharsRead);
				charBuffer.clear();
			}
			return formatSqlBody(builder.toString());
		}
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
	 * loadPathからの相対パスを取得する.
	 * loadPathと一致する部分がなかった場合は、引数のpathの値をそのまま返却する
	 *
	 * @param path 相対パスを取得する元のパス
	 * @return 相対パス
	 */
	private Path relativePath(final Path path) {
		var pathList = StreamSupport.stream(path.spliterator(), false)
				.collect(Collectors.toList());

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
	 * SQLファイルの情報を保持するオブジェクト
	 */
	protected static class SqlInfo {
		/** sqlNameに対応するPath. */
		private final Path path;

		/** 読み込みを行ったSQLルートパス */
		private final Path rootPath;

		/** 読み込んだファイルのscheme */
		private final String scheme;

		/** SQLファイルの内容.*/
		private final String sqlBody;

		/**
		 * コンストラクタ
		 * @param path Path
		 * @param rootPath 読み込みを行ったSQLルートパス
		 * @param scheme 読み込んだファイルのscheme
		 * @param sqlBody SqlBody
		 */
		public SqlInfo(final Path path, final Path rootPath, final String scheme, final String sqlBody) {
			this.path = path;
			this.rootPath = rootPath;
			this.scheme = scheme;
			this.sqlBody = sqlBody;
		}

		/**
		 * sqlNameに対応するPath.を取得します.
		 * @return sqlNameに対応するPath.
		 */
		public Path getPath() {
			return path;
		}

		/**
		 * 読み込みを行ったSQLルートパスを取得します.
		 * @return 読み込みを行ったSQLルートパス
		 */
		public Path getRootPath() {
			return rootPath;
		}

		/**
		 * 読み込んだファイルのschemeを取得します.
		 * @return 読み込んだファイルのscheme
		 */
		public String getScheme() {
			return scheme;
		}

		/**
		 * SQLファイルの内容.を取得します.
		 * @return SQLファイルの内容.
		 */
		public String getSqlBody() {
			return sqlBody;
		}

	}
}
