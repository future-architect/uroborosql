package jp.co.future.uroborosql.store;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * SQL管理実装クラス
 *
 * @author H.Sugimoto
 */
public class SqlManagerImpl implements SqlManager {

	/** SQLキャッシュ */
	private ConcurrentHashMap<String, String> sqlMap = new ConcurrentHashMap<>();

	/** SQL読み込みクラス */
	private SqlLoader sqlLoader;

	/** 起動時にSQLファイルをキャッシュするかどうか */
	private boolean cache = true;

	/** コンストラクタ */
	public SqlManagerImpl() {
		sqlLoader = new SqlLoaderImpl();
	}

	/**
	 * コンストラクタ
	 *
	 * @param sqlLoader SQLローダー
	 */
	public SqlManagerImpl(final SqlLoader sqlLoader) {
		this.sqlLoader = sqlLoader;
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLをロードするルートパス
	 */
	public SqlManagerImpl(final String loadPath) {
		this();
		if (loadPath != null) {
			sqlLoader.setLoadPath(loadPath);
		}
	}

	/**
	 * コンストラクタ
	 *
	 * @param loadPath SQLをロードするルートパス
	 * @param fileExtension SQL拡張子
	 */
	public SqlManagerImpl(final String loadPath, final String fileExtension) {
		this(loadPath);
		if (fileExtension != null) {
			sqlLoader.setFileExtension(fileExtension);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#initialize()
	 */
	@Override
	public void initialize() {
		if (cache) {
			sqlMap = sqlLoader.load();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.store.SqlManager#getSql(java.lang.String)
	 */
	@Override
	public String getSql(final String sqlPath) {
		if (!cache) {
			return sqlLoader.load(sqlPath);
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
			return sqlLoader.existSql(sqlPath);
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
	 * SQL読み込みクラス設定
	 *
	 * @param sqlLoader SqlLoaderオブジェクト
	 */
	public void setSqlLoader(final SqlLoader sqlLoader) {
		this.sqlLoader = sqlLoader;
	}

	/**
	 * SQLファイルロードパス取得<br>
	 *
	 * @return SQLファイルロードパス
	 */
	public String getLoadPath() {
		return sqlLoader.getLoadPath();
	}

	/**
	 * SQLファイルロードパス設定<br>
	 *
	 * @param loadPath SQLファイルロードパス
	 */
	public void setLoadPath(final String loadPath) {
		sqlLoader.setLoadPath(loadPath);
	}

	/**
	 * SQLファイル拡張子取得<br>
	 *
	 * @return SQLファイル拡張子
	 */
	public String getFileExtension() {
		return sqlLoader.getFileExtension();
	}

	/**
	 * SQLファイル拡張子設定<br>
	 *
	 * @param fileExtension SQLファイル拡張子
	 */
	public void setFileExtension(final String fileExtension) {
		sqlLoader.setFileExtension(fileExtension);
	}

	/**
	 * 起動時にSQLファイルをキャッシュするかどうか<BR>
	 *
	 * @param cache SQLをキャッシュする場合<code>true</code>
	 */
	public void setCache(final boolean cache) {
		this.cache = cache;
	}

	/**
	 * 起動時にSQLファイルをキャッシュするかどうかを取得する
	 *
	 * @return SQLをキャッシュする場合<code>true</code>
	 */
	public boolean isCache() {
		return this.cache;
	}

}
