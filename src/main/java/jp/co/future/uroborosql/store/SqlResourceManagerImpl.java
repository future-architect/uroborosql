/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import java.io.IOException;
import java.net.URISyntaxException;
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
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * SQLリソース管理実装クラス
 *
 * @author H.Sugimoto
 */
public class SqlResourceManagerImpl implements SqlResourceManager {
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

	/** 初期化時にSQLを全件ロードするかどうか */
	private final boolean loadAllOnInitialize;

	/** Dialect */
	private Dialect dialect;

	/** sqlNameとそれに対するSqlInfoの紐付きを持つMap */
	protected final ConcurrentHashMap<String, SqlInfo> sqlInfos = new ConcurrentHashMap<>();

	/** SQLリソースローダー */
	private SqlResourceLoader sqlResourceLoader;

	/**
	 * コンストラクタ
	 */
	public SqlResourceManagerImpl() {
		this(DEFAULT_LOAD_PATH, false);
	}

	/**
	 * コンストラクタ
	 * @param loadAllOnInitialize 初期化時にSQLを全件ロードするかどうか
	 */
	public SqlResourceManagerImpl(final boolean loadAllOnInitialize) {
		this(DEFAULT_LOAD_PATH, loadAllOnInitialize);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 */
	public SqlResourceManagerImpl(final String loadPath) {
		this(loadPath, null, false);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param loadAllOnInitialize 初期化時にSQLを全件ロードするかどうか
	 */
	public SqlResourceManagerImpl(final String loadPath, final boolean loadAllOnInitialize) {
		this(loadPath, null, loadAllOnInitialize);
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
	 * @param loadAllOnInitialize 初期化時にSQLを全件ロードするかどうか
	 */
	public SqlResourceManagerImpl(final String loadPath, final String fileExtension,
			final boolean loadAllOnInitialize) {
		this(loadPath, fileExtension, null, loadAllOnInitialize);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 */
	public SqlResourceManagerImpl(final String loadPath, final String fileExtension, final Charset charset) {
		this(List.of(loadPath), fileExtension, charset, false);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 * @param loadAllOnInitialize 初期化時にSQLを全件ロードするかどうか
	 */
	public SqlResourceManagerImpl(final String loadPath, final String fileExtension, final Charset charset,
			final boolean loadAllOnInitialize) {
		this(List.of(loadPath), fileExtension, charset, loadAllOnInitialize);
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
	 * @param loadAllOnInitialize 初期化時にSQLを全件ロードするかどうか
	 */
	public SqlResourceManagerImpl(final List<String> loadPaths, final boolean loadAllOnInitialize) {
		this(loadPaths, null, loadAllOnInitialize);
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
	 * @param loadAllOnInitialize 初期化時にSQLを全件ロードするかどうか
	 */
	public SqlResourceManagerImpl(final List<String> loadPaths, final String fileExtension,
			final boolean loadAllOnInitialize) {
		this(loadPaths, fileExtension, null, loadAllOnInitialize);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLファイルをロードするルートパスのリスト
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 *
	 * @throws IllegalArgumentException loadPathsに<code>null</code>が含まれる場合
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
	 * @param loadAllOnInitialize 初期化時にSQLを全件ロードするかどうか
	 *
	 * @throws IllegalArgumentException loadPathsに<code>null</code>が含まれる場合
	 */
	public SqlResourceManagerImpl(final List<String> loadPaths, final String fileExtension, final Charset charset,
			final boolean loadAllOnInitialize) {
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
		this.loadAllOnInitialize = loadAllOnInitialize;
		this.sqlResourceLoader = new SqlResourceLoaderImpl(this.fileExtension, this.charset);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#initialize()
	 */
	@Override
	public void initialize() {
		if (loadAllOnInitialize) {
			generateAllSqlInfos();
		}
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
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#getSqlResourceLoader()
	 */
	@Override
	public SqlResourceLoader getSqlResourceLoader() {
		return sqlResourceLoader;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceManager#setSqlResourceLoader(jp.co.future.uroborosql.store.SqlResourceLoader)
	 */
	@Override
	public void setSqlResourceLoader(final SqlResourceLoader sqlResourceLoader) {
		this.sqlResourceLoader = sqlResourceLoader;
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
	 * sqlNameとそれに対するSqlInfoのMapを生成する.
	 */
	protected void generateAllSqlInfos() {
		for (var loadPath : this.loadPaths) {
			var sqls = sqlResourceLoader.loadAllSql(loadPath);
			sqls.stream()
					.forEach(sqlInfo -> {
						var path = sqlInfo.getPath();
						if (validPath(path)) {
							var sqlName = getSqlName(path);
							this.sqlInfos.compute(sqlName, remappingSqlInfo(sqlInfo));
						}
					});
			if (LOG.isTraceEnabled()) {
				this.sqlInfos.forEach((sqlName, sqlInfo) -> {
					traceWith(LOG)
							.setMessage("SqlInfo - sqlName : {}, path : {}, rootPath : {}, scheme : {}, sqlBody : {}.")
							.addArgument(sqlName)
							.addArgument(sqlInfo.getPath())
							.addArgument(sqlInfo.getRootPath())
							.addArgument(sqlInfo.getScheme())
							.addArgument(sqlInfo.getSqlBody())
							.log();
				});
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
		var dialectName = dialect.getDatabaseType().toLowerCase();
		for (var loadPath : loadPaths) {
			var path = loadPath.resolve(dialectName).resolve(sqlName + fileExtension);
			var url = sqlResourceLoader.getResource(path);
			if (url == null) {
				path = loadPath.resolve(sqlName + fileExtension);
				url = sqlResourceLoader.getResource(path);
			}
			if (url != null) {
				try {
					var scheme = url.toURI().getScheme().toLowerCase();
					var sqlBody = sqlResourceLoader.loadSql(url);
					var sqlInfo = new SqlInfo(path, loadPath, scheme, sqlBody);
					this.sqlInfos.compute(sqlName, remappingSqlInfo(sqlInfo));
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
	 * 優先度が高いSqlInfoが採用されるようにMapの再配置を行う.
	 * <pre>
	 * 1. Dialect付Pathを優先
	 * 2. schemeはjarよりもfileを優先
	 * 3. rootPathがloadPathsの並び順で前にいるものを優先
	 * </pre>
	 * @param sqlInfo
	 *
	 * @return 優先度判定を行った結果優先となったインスタンスを返すBiFunction
	 */
	private BiFunction<String, SqlInfo, SqlInfo> remappingSqlInfo(final SqlInfo sqlInfo) {
		return (k, v) -> {
			if (v == null) {
				// sqlNameに対するインスタンスがなければ追加
				return sqlInfo;
			} else {
				// Dialect付Pathを優先
				if (isDialectPath(sqlInfo.getPath()) && !isDialectPath(v.getPath())) {
					return sqlInfo;
				}
				// schemeはjarよりもfileを優先
				if (SqlInfo.SCHEME_FILE.equals(sqlInfo.getScheme()) && SqlInfo.SCHEME_JAR.equals(v.getScheme())) {
					return sqlInfo;
				}
				// ロードパスの並び順が先の方を優先
				if (loadPaths.indexOf(sqlInfo.getRootPath()) < loadPaths.indexOf(v.getRootPath())) {
					return sqlInfo;
				}
				// それ以外の場合は現在のインスタンスを適用
				return v;
			}
		};
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
	 * loadPathからの相対パスを取得する.
	 * loadPathと一致する部分がなかった場合は、引数のpathの値をそのまま返却する
	 *
	 * @param path 相対パスを取得する元のパス
	 * @return 相対パス
	 */
	protected Path relativePath(final Path path) {
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
	protected boolean validPath(final Path path) {
		var relativePath = relativePath(path);
		if (relativePath.equals(path)) {
			return true;
		}

		var dialectName = relativePath.getName(0).toString().toLowerCase();
		// loadPathの直下が現在のdialect以外と一致する場合は無効なパスと判定する
		return !dialects.contains(dialectName) || isDialectPath(relativePath);
	}

	/**
	 * Dialect配下のPathかどうかを判定する.
	 * @param path 対象となるPath
	 * @return Dialect配下のPathの場合<code>true</code>
	 */
	protected boolean isDialectPath(final Path path) {
		for (var p : path) {
			if (this.dialect.getDatabaseType().equals(p.toString().toLowerCase())) {
				return true;
			}
		}
		return false;
	}
}
