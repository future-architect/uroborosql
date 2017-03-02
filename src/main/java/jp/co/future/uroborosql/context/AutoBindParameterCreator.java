package jp.co.future.uroborosql.context;

import java.util.Map;

import jp.co.future.uroborosql.parameter.Parameter;

/**
 * Sqlに自動的にバインドするパラメータを生成するためのインタフェース
 * 
 * @author H.Sugimoto
 * 
 */
public interface AutoBindParameterCreator {

	/**
	 * SqlContextに自動的にバインドするパラメータのマップを取得する
	 * 
	 * @return パラメータマップ（key:パラメータ名, value:パラメータ）
	 */
	Map<String, Parameter> getBindParameterMap();
}
