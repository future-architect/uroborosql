package jp.co.future.uroborosql.store;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * SQL読み込みクラス<br>
 *
 * @author H.Sugimoto
 */
public class SqlLoaderImpl implements SqlLoader {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(SqlLoaderImpl.class);

	/** SQLファイルをロードするルートパス */
	private String loadPath = DEFAULT_LOAD_PATH;

	/**
	 * SQLファイル拡張子
	 */
	private String fileExtension = DEFAULT_FILE_EXTENSION;

	/**
	 * コンストラクタ<br>
	 */
	public SqlLoaderImpl() {
	}

	/**
	 * コンストラクタ<br>
	 *
	 * @param loadPath SQLファイルをロードするルートパス
	 * @param fileExtension SQLファイル拡張子
	 */
	public SqlLoaderImpl(final String loadPath, final String fileExtension) {
		setLoadPath(loadPath);
		setFileExtension(fileExtension);
	}

	/**
	 * SQLファイルロードパス取得<br>
	 *
	 * @return SQLファイルロードパス
	 */
	@Override
	public String getLoadPath() {
		return loadPath;
	}

	/**
	 * SQLファイルロードパス設定<br>
	 *
	 * @param loadPath SQLファイルロードパス
	 */
	@Override
	public void setLoadPath(final String loadPath) {
		if (loadPath == null) {
			LOG.warn("SQLファイルロードパスにNULLが設定されたため、デフォルトの値を使用します");
			LOG.warn("デフォルトロードパス[{}]", DEFAULT_LOAD_PATH);
			this.loadPath = DEFAULT_LOAD_PATH;
		} else {
			this.loadPath = loadPath;
		}
	}

	/**
	 * SQLファイル拡張子取得<br>
	 *
	 * @return SQLファイル拡張子
	 */
	@Override
	public String getFileExtension() {
		return fileExtension;
	}

	/**
	 * SQLファイル拡張子設定<br>
	 *
	 * @param fileExtension SQLファイル拡張子
	 */
	@Override
	public void setFileExtension(final String fileExtension) {
		if (fileExtension == null) {
			LOG.warn("SQLファイル拡張子にNULLが設定されたため、デフォルトの値を使用します");
			LOG.warn("デフォルトSQLファイル拡張子[{}]", DEFAULT_FILE_EXTENSION);
			this.fileExtension = DEFAULT_FILE_EXTENSION;
		} else {
			this.fileExtension = fileExtension;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlLoader#load()
	 */
	@Override
	public ConcurrentHashMap<String, String> load() {
		ConcurrentHashMap<String, String> loadedSqlMap = new ConcurrentHashMap<>();
		try {
			Enumeration<URL> resources = Thread.currentThread().getContextClassLoader().getResources(loadPath);
			while (resources.hasMoreElements()) {
				URL resource = resources.nextElement();
				File rootDir = new File(URLDecoder.decode(resource.getFile(), StandardCharsets.UTF_8.toString()));

				if (!rootDir.exists() || !rootDir.isDirectory()) {
					if ("jar".equalsIgnoreCase(resource.getProtocol())) {
						putAllIfAbsent(loadedSqlMap, load((JarURLConnection) resource.openConnection(), loadPath));
						continue;
					}

					LOG.warn("ディレクトリでは無いので無視します[{}]", rootDir.getAbsolutePath());
					continue;
				}

				LOG.debug("SQL定義ファイルの読み込みを開始します[{}]", rootDir.getAbsolutePath());
				putAllIfAbsent(loadedSqlMap, load(new StringBuilder(), rootDir));
			}
		} catch (IOException e) {
			throw new UroborosqlRuntimeException("SQL定義ファイルの読み込みに失敗しました", e);
		}

		if (loadedSqlMap.isEmpty()) {
			LOG.warn("SQL定義ファイルが見つかりませんでした");
			LOG.warn("空のSQLキャッシュを返します");
		}

		return loadedSqlMap;
	}

	/**
	 * キーが存在する場合は上書きを行わずに指定されたマップのすべてのマッピングをこのマップにコピーします
	 *
	 * @param baseMap マージ先の対象データ（マップの内容が更新されます）
	 * @param map コピーの対象データ
	 */
	private void putAllIfAbsent(final Map<String, String> baseMap, final Map<String, String> map) {
		for (Map.Entry<String, String> entry : map.entrySet()) {
			if (!baseMap.containsKey(entry.getKey())) {
				baseMap.put(entry.getKey(), entry.getValue());
			}
		}
	}

	/**
	 * Jar ファイル内の指定のフォルダパス以下にある .sqlファイルを読み込み SQL識別子をキーとしたSQL文のMapを返します
	 *
	 * @param jarUrlConnection Jarファイル内のURLコレクション
	 * @param loadPath ロードパス
	 * @return SQL識別子をキーとしたSQL文のMap
	 */
	private ConcurrentHashMap<String, String> load(final JarURLConnection jarUrlConnection, final String loadPath)
			throws IOException {
		LOG.debug("jar[{}]以下のSQL定義を読み込んでいます", jarUrlConnection);

		ConcurrentHashMap<String, String> sqlMap = new ConcurrentHashMap<>();
		JarFile jarFile = jarUrlConnection.getJarFile();
		Enumeration<JarEntry> jarEnum = jarFile.entries();
		while (jarEnum.hasMoreElements()) {
			JarEntry jarEntry = jarEnum.nextElement();
			String fileName = jarEntry.getName();
			if (fileName.startsWith(loadPath) && fileName.toLowerCase().endsWith(fileExtension)) {
				String sql = trimSlash(read(new BufferedReader(new InputStreamReader(jarFile.getInputStream(jarEntry)))));
				fileName = fileName.substring(loadPath.length() + 1, fileName.length() - 4);
				sqlMap.put(fileName, sql);

				LOG.trace("SQL定義ファイル[{}]を読み込みました", fileName);
				LOG.trace("SQL定義を追加します[{}],[{}]", fileName, sql);
			}
		}
		return sqlMap;

	}

	/**
	 * 指定されたパッケージ以下のSQLを順次読み込みする<br>
	 * 文末の"/"は削除される<br>
	 *
	 * @param packageName パッケージ名を格納するStringBuilder
	 * @param dir 探索対象ディレクトリ
	 * @return SQL識別子をキーとしたSQL文のMap
	 * @throws IOException ファイルアクセスに失敗した場合
	 */
	private ConcurrentHashMap<String, String> load(final StringBuilder packageName, final File dir) throws IOException {
		LOG.debug("パッケージ[{}]以下のSQL定義を読み込んでいます", packageName);

		ConcurrentHashMap<String, String> sqlMap = new ConcurrentHashMap<>();
		File[] files = dir.listFiles();
		for (File file : files) {
			String fileName = file.getName();
			if (file.isDirectory()) {
				sqlMap.putAll(load(makeNewPackageName(packageName, file), file));
			} else if (fileName.toLowerCase().endsWith(fileExtension)) {
				String sql = trimSlash(read(new BufferedReader(new FileReader(file))));
				String sqlName = makeSqlName(packageName, fileName);
				sqlMap.put(sqlName, sql);

				LOG.trace("SQL定義ファイル[{}]を読み込みました", fileName);
				LOG.trace("SQL定義を追加します[{}],[{}]", sqlName, sql);
			}
		}
		return sqlMap;
	}

	/**
	 * ファイルパス指定のSQL読み込み<br>
	 * 文末の"/"は削除される<br>
	 *
	 * @param filePath ルートフォルダからの相対ファイルパス
	 *
	 * @return SQL
	 *
	 * @see jp.co.future.uroborosql.store.SqlLoader#load(java.lang.String)
	 */
	@Override
	public String load(final String filePath) {
		if (StringUtils.isEmpty(filePath)) {
			throw new IllegalArgumentException("ファイルパスが正しくありません。filePath=" + filePath);
		}
		String targetFilePath = getFilePath(trimSqlExtension(filePath.replace(".", PATH_SEPARATOR)));
		String sql = null;

		URL resource = Thread.currentThread().getContextClassLoader().getResource(targetFilePath);

		try (InputStream is = resource.openStream()) {
			if (resource != null) {
				sql = trimSlash(read(new BufferedReader(new InputStreamReader(is))));

				LOG.debug("SQL定義ファイル[{}]を読み込みました", targetFilePath);
			}
		} catch (IOException e) {
			throw new UroborosqlRuntimeException("SQL定義ファイル[" + targetFilePath + "]の読み込みに失敗しました", e);
		}

		if (sql == null) {
			throw new UroborosqlRuntimeException("SQL定義ファイル[" + targetFilePath + "]が見つかりません");
		}
		return sql;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlLoader#existSql(java.lang.String)
	 */
	@Override
	public boolean existSql(final String fileName) {
		return Thread.currentThread().getContextClassLoader().getResource(getFilePath(fileName)) != null;
	}

	/**
	 * ファイルの絶対パスを取得
	 *
	 * @param filePath ルートパスからの相対パス（拡張子はつけない）
	 * @return 絶対パス（拡張子付）
	 */
	private String getFilePath(final String filePath) {
		return loadPath + PATH_SEPARATOR + filePath + fileExtension;
	}

	/**
	 * SQL名作成<br>
	 *
	 * @param packageName パッケージ名
	 * @param filePath ルートパスからの相対パス
	 * @return
	 */
	private String makeSqlName(final StringBuilder packageName, final String filePath) {
		if (packageName.length() == 0) {
			return trimSqlExtension(filePath);
		} else {
			return new StringBuilder(packageName).append(PATH_SEPARATOR).append(trimSqlExtension(filePath))
					.toString();
		}
	}

	/**
	 * SQL定義ファイルの拡張子を除く<br>
	 *
	 * @param filePath ルートパスからの相対パス
	 * @return 拡張子を除去した文字列
	 */
	private String trimSqlExtension(final String filePath) {
		return StringUtils.removeEnd(filePath, fileExtension);
	}

	/**
	 * パッケージ名作成<br>
	 *
	 * @param packageName パッケージ名
	 * @param dir 対象ディレクトリ
	 * @return パッケージ名にディレクトリを付与したStringBuilder
	 */
	private StringBuilder makeNewPackageName(final StringBuilder packageName, final File dir) {
		if (packageName.length() == 0) {
			return new StringBuilder(dir.getName());
		} else {
			return new StringBuilder(packageName).append(PATH_SEPARATOR).append(dir.getName());
		}
	}

	/**
	 * ファイル読み込み<br>
	 *
	 * 以下２種類に対応
	 * <ol>
	 * 	<li>キャッシュ時のディレクトリからFileの読み込む場合</li>
	 * 	<li>キャッシュなしで クラスパス内のリソース(jarファイル内も含む)を読み込む場合</li>
	 * </ol>
	 *
	 * @param reader リーダ
	 * @return SQL文を読み込み 文末のSQLコマンド分割文字(; または / )を除いた文字列を返す
	 * @throws IOException
	 */
	private String read(final BufferedReader reader) throws IOException {
		StringBuilder sqlBuilder = new StringBuilder();
		try {
			for (String line : reader.lines().toArray(String[]::new)) {
				sqlBuilder.append(line).append(System.lineSeparator());
			}
		} finally {
			if (reader != null) {
				reader.close();
			}
		}

		return sqlBuilder.toString();
	}

	/**
	 * SQL文末尾の"/"を取り除く<br>
	 *
	 * @param sql SQL文字列
	 * @return トリム後文字列
	 */
	private String trimSlash(final String sql) {
		String trimedSql = sql.trim();
		if (trimedSql.endsWith(PATH_SEPARATOR) && !trimedSql.endsWith("*/")) {
			return StringUtils.stripEnd(trimedSql, PATH_SEPARATOR);
		} else {
			return sql;
		}
	}
}
