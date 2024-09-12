/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.tx;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.log.support.ServiceLoggingSupport;

public interface TransactionContext extends AutoCloseable, ServiceLoggingSupport {

	/**
	 * SqlConfigの取得.
	 * @return SqlConfig
	 */
	SqlConfig getSqlConfig();

	/**
	 * コネクションの取得.
	 *
	 * @return コネクション
	 * @throws SQLException SQL例外
	 */
	Connection getConnection() throws SQLException;

	/**
	 * ConnectionContextの取得.
	 * @return ConnectionContext
	 */
	ConnectionContext getConnectionContext();

	/**
	 * ステートメント取得.
	 *
	 * @param executionContext ExecutionContext
	 * @return PreparedStatement
	 * @throws SQLException SQL例外
	 */
	PreparedStatement getPreparedStatement(ExecutionContext executionContext) throws SQLException;

	/**
	 * Callableステートメント初期化.
	 *
	 * @param executionContext ExecutionContext
	 * @return CallableStatement
	 * @throws SQLException SQL例外
	 */
	CallableStatement getCallableStatement(ExecutionContext executionContext) throws SQLException;

	/**
	 * トランザクションのコミット.
	 */
	void commit();

	/**
	 * トランザクションのロールバック.
	 */
	void rollback();

	/**
	 * 現在のトランザクションがロールバック指定になっているかを取得.
	 *
	 * @return 現在のトランザクションがロールバック指定になっているか
	 */
	boolean isRollbackOnly();

	/**
	 * 現在のトランザクションをロールバックすることを予約します.
	 */
	void setRollbackOnly();

	/**
	 * 指定されたセーブポイントが設定されたあとに行われたすべての変更をロールバックします.
	 *
	 * <pre>
	 *  この処理は{@link Connection#rollback(java.sql.Savepoint)}の処理に依存します.
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 */
	void rollback(String savepointName);

	/**
	 * トランザクションのセーブポイントを作成します.
	 *
	 * <pre>
	 *  この処理は{@link Connection#setSavepoint(String)}の処理に依存します.
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 */
	void setSavepoint(String savepointName);

	/**
	 * トランザクションから指定されたセーブポイントと以降のセーブポイントを削除します.
	 *
	 * <pre>
	 *  この処理は{@link Connection#releaseSavepoint(java.sql.Savepoint)}の処理に依存します.
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 */
	void releaseSavepoint(String savepointName);

	/**
	 * {@link #setSavepoint(String)}で設定したsavepoint名の一覧を取得する.
	 *
	 * @return savepoint名の一覧
	 */
	List<String> getSavepointNames();

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see java.lang.AutoCloseable#close()
	 */
	@Override
	void close();

}