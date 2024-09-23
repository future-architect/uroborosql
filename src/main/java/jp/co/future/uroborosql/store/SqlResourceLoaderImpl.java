/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.JarURLConnection;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.log.support.ServiceLoggingSupport;
import jp.co.future.uroborosql.utils.ObjectUtils;

/**
 * SQLリソースローダーのデフォルト実装
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class SqlResourceLoaderImpl implements SqlResourceLoader, ServiceLoggingSupport {
	/** CharBufferのキャパシティ. */
	private static final int BUFFER_CAPACITY = 8 * 1024;

	/** SQLファイル拡張子 */
	private String fileExtension;

	/** SQLファイルエンコーディング */
	private Charset charset;

	/**
	 * コンストラクタ
	 */
	public SqlResourceLoaderImpl() {
		this(".sql", Charset.forName(Charset.defaultCharset().displayName()));
	}

	/**
	 * コンストラクタ
	 * @param fileExtension SQLファイル拡張子
	 * @param charset SQLファイルエンコーディング
	 */
	public SqlResourceLoaderImpl(final String fileExtension, final Charset charset) {
		this.fileExtension = fileExtension;
		this.charset = charset;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceLoader#getFileExtension()
	 */
	@Override
	public String getFileExtension() {
		return fileExtension;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceLoader#setFileExtension(java.lang.String)
	 */
	@Override
	public void setFileExtension(final String fileExtension) {
		this.fileExtension = fileExtension;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceLoader#getCharset()
	 */
	@Override
	public Charset getCharset() {
		return charset;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceLoader#setCharset(java.nio.charset.Charset)
	 */
	@Override
	public void setCharset(final Charset charset) {
		this.charset = charset;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceLoader#getClassLoader()
	 */
	@Override
	public ClassLoader getClassLoader() {
		return Thread.currentThread().getContextClassLoader();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceLoader#getResource(java.lang.String)
	 */
	@Override
	public URL getResource(final String name) {
		return getClassLoader().getResource(name);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceLoader#getResource(java.nio.file.Path)
	 */
	@Override
	public URL getResource(final Path path) {
		return getResource(path.toString());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceLoader#loadAllSql(java.nio.file.Path)
	 */
	@Override
	public List<SqlInfo> loadAllSql(final Path rootPath) {
		return getClassLoader().resources(rootPath.toString().replace('\\', '/'))
				.flatMap(url -> {
					try {
						var scheme = url.toURI().getScheme();
						if (SqlInfo.SCHEME_FILE.equalsIgnoreCase(scheme)) {
							return traverseFile(url, rootPath).stream();
						} else if (SqlInfo.SCHEME_JAR.equalsIgnoreCase(scheme)) {
							return traverseJar(url, rootPath).stream();
						} else {
							warnWith(LOG)
									.setMessage("Unsupported scheme. scheme : {}, url : {}")
									.addArgument(scheme)
									.addArgument(url)
									.log();
						}
						return null;
					} catch (IOException | URISyntaxException ex) {
						errorWith(LOG)
								.setMessage("Can't load sql files.")
								.setCause(ex)
								.log();
						throw new UroborosqlRuntimeException("I/O error occurred.", ex);
					}
				})
				.filter(Objects::nonNull)
				.collect(Collectors.toList());
	}

	/**
	 * 指定されたURL配下のファイルを順次追跡し、PathとSQL本文のMapを返却する.
	 *
	 * @param url 追跡を行うディレクトリ、またはファイルのURL
	 * @param rootPath 読み込みを行ったSQLルートパス
	 * @return PathとSQL本文を格納したMap
	 * @throws IOException SQLの読み込みに失敗した場合
	 */
	private List<SqlInfo> traverseFile(final URL url, final Path rootPath) throws IOException {
		traceWith(LOG)
				.setMessage("traverseFile start. url : {}.")
				.addArgument(url)
				.log();
		try {
			var path = Path.of(url.toURI());
			if (Files.notExists(path)) {
				List.of();
			}
			var sqlInfos = new ArrayList<SqlInfo>();
			if (Files.isDirectory(path)) {
				try (var ds = Files.newDirectoryStream(path)) {
					for (var child : ds) {
						sqlInfos.addAll(traverseFile(child.toUri().toURL(), rootPath));
					}
				} catch (IOException ex) {
					throw new UroborosqlRuntimeException("I/O error occurred.", ex);
				}
			} else if (path.toString().endsWith(getFileExtension())) {
				var sqlBody = loadSql(url.openStream());
				sqlInfos.add(new SqlInfo(path, rootPath, SqlInfo.SCHEME_FILE, sqlBody));
			}
			return sqlInfos;
		} catch (URISyntaxException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * 指定されたjarのURL配下のファイルを順次追跡し、PathとSQL本文のMapを返却する.
	 *
	 * @param url 追跡を行うディレクトリ、またはファイルのURL
	 * @param rootPath 読み込みを行ったSQLルートパス
	 * @return PathとSQL本文を格納したMap
	 * @throws IOException SQLの読み込みに失敗した場合
	 */
	private List<SqlInfo> traverseJar(final URL url, final Path rootPath) throws IOException {
		var conn = (JarURLConnection) url.openConnection();
		try (var jarFile = conn.getJarFile()) {
			return jarFile.stream()
					.map(jarEntry -> {
						try {
							var name = jarEntry.getName();
							if (!jarEntry.isDirectory() && name.endsWith(getFileExtension())) {
								var path = Paths.get(name);
								var sqlBody = loadSql(jarFile.getInputStream(jarEntry));
								return new SqlInfo(path, rootPath, SqlInfo.SCHEME_JAR, sqlBody);
							} else if (jarEntry.isDirectory()) {
								traceWith(LOG)
										.setMessage("traverseJar start. name : {}.")
										.addArgument(name)
										.log();
								return null;
							} else {
								return null;
							}
						} catch (IOException ex) {
							throw new UroborosqlRuntimeException("I/O error occurred.", ex);
						}
					})
					.filter(Objects::nonNull)
					.collect(Collectors.toList());
		} catch (UroborosqlRuntimeException ex) {
			throw (IOException) ex.getCause();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceLoader#loadSql(java.net.URL)
	 */
	@Override
	public String loadSql(final URL url) throws IOException {
		return loadSql(url.openStream());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlResourceLoader#loadSql(java.io.InputStream)
	 */
	@Override
	public String loadSql(final InputStream is) throws IOException {
		try (var reader = new BufferedReader(new InputStreamReader(is, getCharset()))) {
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
	private String formatSqlBody(final String sqlBody) {
		var newBody = sqlBody.trim();
		if (newBody.endsWith("/") && !newBody.endsWith("*/")) {
			newBody = ObjectUtils.removeEnd(newBody, "/");
		} else {
			newBody = newBody + System.lineSeparator();
		}
		return newBody;
	}
}
