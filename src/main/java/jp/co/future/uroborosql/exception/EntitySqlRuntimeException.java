package jp.co.future.uroborosql.exception;

import java.sql.SQLException;

/**
 * ORMの実行エラーでスローされる例外
 *
 * @author ota
 */
public class EntitySqlRuntimeException extends UroborosqlRuntimeException {
	/**
	 * Entity処理種別
	 */
	public enum EntityProcKind {
		/** INSERT */
		INSERT,
		/** UPDATE */
		UPDATE,
		/** DELETE */
		DELETE,
		/** SELECT */
		SELECT,
	}

	private final EntityProcKind procKind;

	/**
	 * コンストラクタ
	 *
	 * @param procKind 処理種別
	 * @param cause SQLException
	 */
	public EntitySqlRuntimeException(final EntityProcKind procKind, final SQLException cause) {
		super(cause);
		this.procKind = procKind;
	}

	/**
	 * Exceptionの発生した処理種別
	 *
	 * @return 処理種別
	 */
	public EntityProcKind getProcKind() {
		return this.procKind;
	}

}
