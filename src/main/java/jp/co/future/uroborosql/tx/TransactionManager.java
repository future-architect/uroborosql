/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.tx;

import java.sql.Connection;

import jp.co.future.uroborosql.connection.ConnectionManager;

/**
 * トランザクションマネージャ
 *
 * @author ota
 */
public interface TransactionManager extends ConnectionManager {

	/**
	 * トランザクションを実行します。
	 *
	 * @param runnable トランザクション内で実行する処理
	 */
	void required(SQLRunnable runnable);

	/**
	 * トランザクションを実行します。
	 *
	 * @param supplier トランザクション内で実行する処理
	 * @param <R> 結果の型
	 * @return 処理の結果
	 */
	<R> R required(SQLSupplier<R> supplier);

	/**
	 * 新たなトランザクションを実行します。
	 *
	 * @param runnable トランザクション内で実行する処理
	 */
	void requiresNew(SQLRunnable runnable);

	/**
	 * 新たなトランザクションを実行します。
	 *
	 * @param supplier トランザクション内で実行する処理
	 * @param <R> 結果の型
	 * @return 処理の結果
	 */
	<R> R requiresNew(SQLSupplier<R> supplier);

	/**
	 * トランザクションを開始せず処理を実行します。
	 *
	 * @param runnable 実行する処理
	 */
	void notSupported(SQLRunnable runnable);

	/**
	 * トランザクションを開始せず処理を実行します。
	 *
	 * @param supplier 実行する処理
	 * @param <R> 結果の型
	 * @return 処理の結果
	 */
	<R> R notSupported(SQLSupplier<R> supplier);

	/**
	 * 現在のトランザクションをロールバックすることを予約します。
	 */
	void setRollbackOnly();

	/**
	 * トランザクションのセーブポイントを作成します。
	 *
	 * <pre>
	 *  この処理は{@link Connection#setSavepoint(String)}の処理に依存します。
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 */
	void setSavepoint(String savepointName);

	/**
	 * トランザクションから指定されたセーブポイントと以降のセーブポイントを削除します。
	 *
	 * <pre>
	 *  この処理は{@link Connection#releaseSavepoint(java.sql.Savepoint)}の処理に依存します。
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 */
	void releaseSavepoint(String savepointName);

	/**
	 * 指定されたセーブポイントが設定されたあとに行われたすべての変更をロールバックします。
	 *
	 * <pre>
	 *  この処理は{@link Connection#rollback(java.sql.Savepoint)}の処理に依存します。
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 */
	void rollback(String savepointName);

	/**
	 * セーブポイントを設定した上でsupplierの内容を実行する.<br>
	 * 処理が成功した場合はセーブポイントを開放し、失敗した場合は設定したセーブポイントまでロールバックする.
	 *
	 * @param supplier 実行する処理
	 * @param <R> 結果の型
	 * @return supplierの処理結果
	 */
	<R> R savepointScope(SQLSupplier<R> supplier);

	/**
	 * セーブポイントを設定した上でrunnableの内容を実行する.<br>
	 * 処理が成功した場合はセーブポイントを開放し、失敗した場合は設定したセーブポイントまでロールバックする.
	 *
	 * @param runnable 実行する処理
	 */
	void savepointScope(SQLRunnable runnable);
}