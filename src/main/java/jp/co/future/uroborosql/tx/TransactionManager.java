package jp.co.future.uroborosql.tx;

import java.sql.Connection;
import java.sql.SQLException;

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
	 * @throws SQLException SQL例外
	 */
	void required(SQLRunnable runnable) throws SQLException;

	/**
	 * トランザクションを実行します。
	 *
	 * @param supplier トランザクション内で実行する処理
	 * @param <R> 結果の型
	 * @return 処理の結果
	 * @throws SQLException SQL例外
	 */
	<R> R required(SQLSupplier<R> supplier) throws SQLException;

	/**
	 * 新たなトランザクションを実行します。
	 *
	 * @param runnable トランザクション内で実行する処理
	 * @throws SQLException SQL例外
	 */
	void requiresNew(SQLRunnable runnable) throws SQLException;

	/**
	 * 新たなトランザクションを実行します。
	 *
	 * @param supplier トランザクション内で実行する処理
	 * @param <R> 結果の型
	 * @return 処理の結果
	 * @throws SQLException SQL例外
	 */
	<R> R requiresNew(SQLSupplier<R> supplier) throws SQLException;

	/**
	 * トランザクションを開始せず処理を実行します。
	 *
	 * @param runnable 実行する処理
	 * @throws SQLException SQL例外
	 */
	void notSupported(SQLRunnable runnable) throws SQLException;

	/**
	 * トランザクションを開始せず処理を実行します。
	 *
	 * @param supplier 実行する処理
	 * @param <R> 結果の型
	 * @return 処理の結果
	 * @throws SQLException SQL例外
	 */
	<R> R notSupported(SQLSupplier<R> supplier) throws SQLException;

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
	 * @throws SQLException SQL例外
	 */
	void setSavepoint(String savepointName) throws SQLException;

	/**
	 * トランザクションから指定されたセーブポイントと以降のセーブポイントを削除します。
	 *
	 * <pre>
	 *  この処理は{@link Connection#releaseSavepoint(java.sql.Savepoint)}の処理に依存します。
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 * @throws SQLException SQL例外
	 */
	void releaseSavepoint(String savepointName) throws SQLException;

	/**
	 * 指定されたセーブポイントが設定されたあとに行われたすべての変更をロールバックします。
	 *
	 * <pre>
	 *  この処理は{@link Connection#rollback(java.sql.Savepoint)}の処理に依存します。
	 * </pre>
	 *
	 * @param savepointName セーブポイントの名前
	 * @throws SQLException SQL例外
	 */
	void rollback(String savepointName) throws SQLException;

}