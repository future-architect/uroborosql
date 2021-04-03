/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.mapping.annotations.GeneratedValue;
import jp.co.future.uroborosql.mapping.annotations.SequenceGenerator;
import jp.co.future.uroborosql.mapping.annotations.Transient;
import jp.co.future.uroborosql.mapping.annotations.Version;

/**
 * カラムマッピングインターフェース
 *
 * @author ota
 */
public interface MappingColumn {

	/**
	 * エンティティから値を取得
	 *
	 * @param entity エンティティ
	 * @return 取得した値
	 */
	Object getValue(Object entity);

	/**
	 * エンティティに値をセット
	 *
	 * @param entity エンティティ
	 * @param value 値
	 */
	void setValue(Object entity, Object value);

	/**
	 * カラム名取得
	 *
	 * @return カラム名
	 */
	String getName();

	/**
	 * キャメルケースカラム名取得
	 *
	 * @return キャメルケースカラム名
	 */
	String getCamelName();

	/**
	 * {@link JavaType}取得
	 *
	 * @return {@link JavaType}
	 */
	JavaType getJavaType();

	/**
	 * IDアノテーションが付与されているかどうか
	 *
	 * @return IDアノテーションが付与されている場合<code>true</code>
	 */
	boolean isId();

	/**
	 * {@link GeneratedValue}の取得
	 *
	 * @return {@link GeneratedValue}
	 */
	GeneratedValue getGeneratedValue();

	/**
	 * {@link SequenceGenerator}の取得
	 *
	 * @return {@link SequenceGenerator}
	 */
	SequenceGenerator getSequenceGenerator();

	/**
	 * 修飾済みのシーケンス名の取得
	 *
	 * @return {@link SequenceGenerator} をもとに修飾したシーケンス名
	 */
	default String getQualifiedSequenceName() {
		var generator = getSequenceGenerator();
		if (generator == null) {
			return "";
		}
		var builder = new StringBuilder();
		if (!"".equals(generator.catalog())) {
			builder.append(generator.catalog()).append(".");
		}
		if (!"".equals(generator.schema())) {
			builder.append(generator.schema()).append(".");
		}
		builder.append(generator.sequence());
		return builder.toString();
	}

	/**
	 * {@link Transient}の取得
	 *
	 * @return {@link Transient}
	 */
	Transient getTransient();

	/**
	 * 指定したSQL種別でtransientかどうかを判断する
	 *
	 * @param sqlKind SQL種別
	 * @return 指定したSQL種別でtransientの場合<code>true</code>
	 */
	boolean isTransient(SqlKind sqlKind);

	/**
	 * バージョン情報カラムかどうか
	 *
	 * @return バージョンカラムの場合は<code>true</code>
	 */
	boolean isVersion();

	/**
	 * {@link Version} の取得
	 *
	 * @return {@link Version}
	 */
	Version getVersion();
}
