/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.store;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * SQL管理実装クラス
 *
 * @author H.Sugimoto
 */
public class SqlManagerImpl implements SqlManager {

	/** SQLキャッシュ */
	private final ConcurrentHashMap<String, String> sqlMap = new ConcurrentHashMap<>();

	/** SQL読み込みクラス */
	private final List<SqlLoader> sqlLoaders = new ArrayList<>();

	/** SQLファイル拡張子 */
	private String fileExtension;

	/** 起動時にSQLファイルをキャッシュするかどうか */
	private boolean cache = true;

	private Dialect dialect;

	/** コンストラクタ */
	public SqlManagerImpl() {
		this(new SqlLoaderImpl());
	}

	/**
	 * コンストラクタ
	 *
	 * @param sqlLoader SQLローダー
	 */
	public SqlManagerImpl(final SqlLoader sqlLoader) {
		this.sqlLoaders.add(sqlLoader);
		this.fileExtension = sqlLoader.getFileExtension();
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLをロードするルートパス
	 */
	public SqlManagerImpl(final String loadPath) {
		SqlLoader sqlLoader = new SqlLoaderImpl();
		if (loadPath != null) {
			sqlLoader.setLoadPath(loadPath);
		}
		this.sqlLoaders.add(sqlLoader);
		this.fileExtension = sqlLoader.getFileExtension();
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLをロードするルートパス
	 * @param fileExtension SQL拡張子
	 */
	public SqlManagerImpl(final String loadPath, final String fileExtension) {
		SqlLoader sqlLoader = new SqlLoaderImpl();
		if (loadPath != null) {
			sqlLoader.setLoadPath(loadPath);
		}
		if (fileExtension != null) {
			sqlLoader.setFileExtension(fileExtension);
		}
		this.sqlLoaders.add(sqlLoader);
		this.fileExtension = sqlLoader.getFileExtension();
	}

	/**
	 * コンストラクタ
	 *
	 * @param cache 起動時にSQLファイルをキャッシュするかどうか
	 */
	public SqlManagerImpl(final boolean cache) {
		this();
		this.cache = cache;
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLをロードするルートパス
	 * @param cache 起動時にSQLファイルをキャッシュするかどうか
	 */
	public SqlManagerImpl(final String loadPath, final boolean cache) {
		this(loadPath);
		this.cache = cache;
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLをロードするルートパスのリスト
	 */
	public SqlManagerImpl(final List<String> loadPaths) {
		this(loadPaths, null);
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPaths SQLをロードするルートパスのリスト
	 * @param fileExtension SQL拡張子
	 */
	public SqlManagerImpl(final List<String> loadPaths, final String fileExtension) {
		if (loadPaths == null || loadPaths.isEmpty()) {
			throw new IllegalArgumentException("loadPaths is required. loadPaths=" + loadPaths);
		}

		for (String loadPath : loadPaths) {
			if (loadPath != null) {
				if (fileExtension != null) {
					this.sqlLoaders.add(new SqlLoaderImpl(loadPath, fileExtension));
				} else {
					this.sqlLoaders.add(new SqlLoaderImpl(loadPath));
				}
			}
		}
		this.fileExtension = this.sqlLoaders.get(0).getFileExtension();

	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#initialize()
	 */
	@Override
	public void initialize() {
		if (cache) {
			for (SqlLoader sqlLoader : sqlLoaders) {
				sqlMap.putAll(sqlLoader.load());
			}
		} else {
			sqlMap.clear();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#shutdown()
	 */
	@Override
	public void shutdown() {
		sqlMap.clear();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#getSql(java.lang.String)
	 */
	@Override
	public String getSql(final String sqlPath) {
		if (!cache) {
			UroborosqlRuntimeException cacheException = null;
			for (SqlLoader sqlLoader : sqlLoaders) {
				try {
					return sqlLoader.load(sqlPath);
				} catch (UroborosqlRuntimeException ex) {
					cacheException = ex;
				}
			}
			throw cacheException;
		} else {
			return sqlMap.get(sqlPath.replace(".", "/"));
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#existSql(java.lang.String)
	 */
	@Override
	public boolean existSql(final String sqlPath) {
		if (!cache) {
			var exists = false;
			for (SqlLoader sqlLoader : sqlLoaders) {
				exists = exists || sqlLoader.existSql(sqlPath);
			}
			return exists;
		} else {
			return sqlMap.get(sqlPath.replace(".", "/")) != null;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#getSqlPathList()
	 */
	@Override
	public List<String> getSqlPathList() {
		return Collections.list(sqlMap.keys());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#getSqlLoader()
	 */
	@Override
	public SqlLoader getSqlLoader() {
		return this.sqlLoaders.get(0);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#setSqlLoader(jp.co.future.uroborosql.store.SqlLoader)
	 */
	@Override
	public void setSqlLoader(final SqlLoader sqlLoader) {
		this.sqlLoaders.set(0, sqlLoader);
	}

	/**
	 * SQLファイルロードパス取得<br>
	 *
	 * @return SQLファイルロードパス
	 */
	public String getLoadPath() {
		return sqlLoaders.get(0).getLoadPath();
	}

	/**
	 * SQLファイルロードパス設定<br>
	 *
	 * @param loadPath SQLファイルロードパス
	 */
	public void setLoadPath(final String loadPath) {
		sqlLoaders.get(0).setLoadPath(loadPath);
	}

	/**
	 * SQLファイル拡張子取得<br>
	 *
	 * @return SQLファイル拡張子
	 */
	public String getFileExtension() {
		return this.fileExtension;
	}

	/**
	 * SQLファイル拡張子設定<br>
	 *
	 * @param fileExtension SQLファイル拡張子
	 */
	public void setFileExtension(final String fileExtension) {
		this.fileExtension = fileExtension;
		for (SqlLoader sqlLoader : sqlLoaders) {
			sqlLoader.setFileExtension(fileExtension);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#isCache()
	 */
	@Override
	public boolean isCache() {
		return this.cache;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#setCache(boolean)
	 */
	@Override
	public void setCache(final boolean cache) {
		this.cache = cache;
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

}
