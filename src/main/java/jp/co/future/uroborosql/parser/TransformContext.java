/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parser;

import java.util.List;

import jp.co.future.uroborosql.parameter.Parameter;

/**
 * SQL変換用コンテキストインタフェース
 *
 * @author H.Sugimoto
 */
public interface TransformContext {

	/**
	 * TransformContextのコピー<br>
	 * BEGIN句を用いた再帰的な解析で使用。
	 *
	 * @return コピーしたTransformContext
	 */
	TransformContext copyTransformContext();

	/**
	 * パラメータ取得。<br>
	 * パラメータキーがBeanの形式(beanName.propertyName)の場合は サブパラメータオブジェクトを生成して返す。
	 * サブパラメータのキーはbeanName.propertyName、値はBeanのフィールド値となる。
	 *
	 * @param paramName パラメータ名
	 * @return パラメータ
	 */
	Parameter getParam(String paramName);

	/**
	 * 実行時SQL取得
	 *
	 * @return 実行k時SQL
	 */
	String getExecutableSql();

	/**
	 * SQLへの追記を行う
	 *
	 * @param sqlPart 追記するSQLの構成要素
	 * @return TransformContext
	 */
	TransformContext addSqlPart(String sqlPart);

	/**
	 * 有効なSQLかどうかの判定
	 *
	 * @return 有効なSQLの場合<code>true</code>
	 */
	boolean isEnabled();

	/**
	 * SQL有効フラグ設定
	 *
	 * @param enabled 有効なSQLの場合<code>true</code>
	 */
	void setEnabled(boolean enabled);

	/**
	 * バインド変数名追加。
	 *
	 * @param bindName バインド変数名
	 * @return TransformContext
	 */
	TransformContext addBindName(String bindName);

	/**
	 * バインド変数名一括追加。
	 *
	 * @param bindNames バインド変数名のリスト
	 * @return TransformContext
	 */
	TransformContext addBindNames(List<String> bindNames);

	/**
	 * バインドパラメータの追加<br>
	 *
	 * @param <V> 値の型
	 * @param paramName パラメータ名
	 * @param value 値
	 * @return T
	 */
	<V> TransformContext param(String paramName, V value);

	/**
	 * バインド変数名のリストを取得する
	 *
	 * @return バインド変数名のリスト
	 */
	List<String> getBindNames();

	/**
	 * バインド変数追加。
	 *
	 * @param bindValiable バインド変数
	 * @return TransformContext
	 */
	TransformContext addBindVariable(Object bindValiable);

	/**
	 * バインド変数一括追加。
	 *
	 * @param bindValiables バインド変数配列
	 * @return TransformContext
	 */
	TransformContext addBindVariables(Object[] bindValiables);

	/**
	 * バインド変数配列取得。
	 *
	 * @return バインド変数配列
	 */
	Object[] getBindVariables();

}
