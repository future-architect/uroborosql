package jp.co.future.uroborosql.enums;

/**
 * 複数件のINSERTの処理方法
 */
public enum InsertsType {
	/**
	 * BULK INSERT<br>
	 * e.g. {@code INSERT INTO ... VALUES ( ... ), ( ... )}<br>
	 * この形式をサポートしていないDatabase方言の場合自動的に{@link #BATCH}の処理に切り替えられます。
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsBulkInsert()
	 */
	BULK,
	/**
	 * BATCH INSERT<br>
	 * {@link java.sql.PreparedStatement#executeBatch()}で処理します。
	 */
	BATCH
}
