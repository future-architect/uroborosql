package jp.co.future.uroborosql.context;

import java.util.List;
import java.util.Map;

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
	 */
	void setConstParamPrefix(String constParamPrefix);

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
	 */
	void setConstantClassNames(List<String> constantClassNames);

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
	 */
	void setEnumConstantPackageNames(List<String> enumConstantPackageNames);

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
	List<AutoBindParameterCreator> getAutoBindParameterCreators();

	/**
	 * 自動バインド用パラメータ生成クラスのリストを設定します。
	 *
	 * @param autoBindParameterCreators
	 *            自動バインド用パラメータ生成クラスのリスト
	 */
	void setAutoBindParameterCreators(List<AutoBindParameterCreator> autoBindParameterCreators);

	/**
	 * パラメータ変換クラス{@link BindParameterMapper}を追加
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 */
	void addBindParamMapper(BindParameterMapper<?> parameterMapper);

	/**
	 * パラメータ変換クラス{@link BindParameterMapper}をremove
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 */
	void removeBindParamMapper(BindParameterMapper<?> parameterMapper);

}
