/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * 動的に生成するテーブルの情報を保持したクラス
 *
 * @author H.Sugimoto
 */
public class MetaTable implements Table {
	/** テーブル名 */
	private final String name;
	/** スキーマ名 */
	private final String schema;
	/** バージョンカラム名 */
	private final String versionColumnName;
	/** 楽観ロックを提供するクラス名 */
	private final String optimisticLockSupplierClassName;

	/**
	 * コンストラクタ
	 *
	 * @param name テーブル名
	 * @param schema スキーマ名
	 * @param versionColumnName バージョンカラム名
	 * @param optimisticLockSupplierClassName 楽観ロックを提供するクラス名
	 */
	public MetaTable(final String name, final String schema, final String versionColumnName,
			final String optimisticLockSupplierClassName) {
		this.name = name;
		this.schema = schema;
		this.versionColumnName = versionColumnName;
		this.optimisticLockSupplierClassName = optimisticLockSupplierClassName;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.Table#getName()
	 */
	@Override
	public String getName() {
		return this.name;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.Table#getSchema()
	 */
	@Override
	public String getSchema() {
		return this.schema;
	}

	/**
	 * バージョンカラムの取得.
	 *
	 * @return バージョンカラム
	 */
	public String getVersionColumnName() {
		return this.versionColumnName;
	}

	/**
	 * 楽観ロックを提供するクラスの取得
	 *
	 * @return 楽観ロックを提供するクラス
	 */
	@SuppressWarnings("unchecked")
	public Class<? extends OptimisticLockSupplier> getOptimisticLockType() {
		try {
			return (Class<? extends OptimisticLockSupplier>) Class.forName(this.optimisticLockSupplierClassName);
		} catch (ClassNotFoundException ex) {
			throw new UroborosqlRuntimeException(
					"OptimisticLockSupplier class : " + this.optimisticLockSupplierClassName + " not found.", ex);
		}
	}
}
