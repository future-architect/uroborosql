package jp.co.future.uroborosql.context;

import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;

/**
 * SQLコンテキストファクトリインタフェース
 *
 * @author H.Sugimoto
 *
 */
public interface SqlContextFactory {
	/** BEAN名 */
	final String FACTORY_BEAN_NAME = "sqlContextFactory";

	/**
	 * 初期化処理
	 */
	void initialize();

	/**
	 * SQLコンテキスト生成メソッド
	 *
	 * @return SQLコンテキスト
	 */
	SqlContext createSqlContext();

	/**
	 * SQLフィルタ管理クラスを取得する
	 *
	 * @return SQLフィルタ管理クラス
	 */
	SqlFilterManager getSqlFilterManager();

	/**
	 * SQLフィルタ管理クラスを設定する
	 *
	 * @param sqlFilterManager SQLフィルタ管理クラス
	 */
	void setSqlFilterManager(SqlFilterManager sqlFilterManager);

	/**
	 * 定数パラメータプレフィックスを取得します。
	 *
	 * @return 定数パラメータプレフィックス
	 */
	String getConstParamPrefix();

	/**
	 * 定数パラメータプレフィックスを設定します。
	 *
	 * @param constParamPrefix
	 *            定数パラメータプレフィックス
	 * @return SqlContextFactory
	 */
	SqlContextFactory setConstParamPrefix(String constParamPrefix);

	/**
	 * 定数クラス名（FQDN）を取得します。
	 *
	 * @return 定数クラス名（FQDN）
	 */
	List<String> getConstantClassNames();

	/**
	 * 定数クラス名（FQDN）を設定します。
	 *
	 * @param constantClassNames
	 *            定数クラス名（FQDN）
	 * @return SqlContextFactory
	 */
	SqlContextFactory setConstantClassNames(List<String> constantClassNames);

	/**
	 * Enum定数パッケージ名を取得します。
	 *
	 * @return Enum定数パッケージ名
	 */
	List<String> getEnumConstantPackageNames();

	/**
	 * Enum定数パッケージ名（FQDN）を設定します。
	 *
	 * @param enumConstantPackageNames Enum定数パッケージ名
	 * @return SqlContextFactory
	 */
	SqlContextFactory setEnumConstantPackageNames(List<String> enumConstantPackageNames);

	/**
	 * 定数クラスパラメータマップを取得します。
	 *
	 * @return 定数クラスパラメータマップ
	 */
	Map<String, Parameter> getConstParameterMap();

	/**
	 * 自動バインド用パラメータ生成クラスのリストを取得します。
	 *
	 * @return 自動バインド用パラメータ生成クラスのリスト
	 */
	@Deprecated
	List<AutoBindParameterCreator> getAutoBindParameterCreators();

	/**
	 * 自動バインド用パラメータ生成クラスのリストを設定します。
	 *
	 * use {@link #addAutoParameterBinder(Consumer)}
	 *
	 * @param autoBindParameterCreators
	 *            自動バインド用パラメータ生成クラスのリスト
	 * @return SqlContextFactory
	 */
	@Deprecated
	SqlContextFactory setAutoBindParameterCreators(List<AutoBindParameterCreator> autoBindParameterCreators);

	/**
	 * 自動パラメータバインド関数(query用)の追加
	 * @param binder 自動パラメータバインド関数
	 * @return SqlContextFactory
	 */
	SqlContextFactory addQueryAutoParameterBinder(Consumer<SqlContext> binder);

	/**
	 * 自動パラメータバインド関数(query用)の削除
	 * @param binder 自動パラメータバインド関数
	 * @return SqlContextFactory
	 */
	SqlContextFactory removeQueryAutoParameterBinder(Consumer<SqlContext> binder);

	/**
	 * 自動パラメータバインド関数(update/batch/proc用)の追加
	 * @param binder 自動パラメータバインド関数
	 * @return SqlContextFactory
	 */
	SqlContextFactory addUpdateAutoParameterBinder(Consumer<SqlContext> binder);

	/**
	 * 自動パラメータバインド関数(update/batch/proc用)の削除
	 * @param binder 自動パラメータバインド関数
	 * @return SqlContextFactory
	 */
	SqlContextFactory removeUpdateAutoParameterBinder(Consumer<SqlContext> binder);

	/**
	 * パラメータ変換クラス{@link BindParameterMapper}を追加
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 * @return SqlContextFactory
	 */
	SqlContextFactory addBindParamMapper(BindParameterMapper<?> parameterMapper);

	/**
	 * パラメータ変換クラス{@link BindParameterMapper}をremove
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 * @return SqlContextFactory
	 */
	SqlContextFactory removeBindParamMapper(BindParameterMapper<?> parameterMapper);

}
