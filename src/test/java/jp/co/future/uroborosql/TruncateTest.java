package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;

/**
 * truncateメソッドのテスト
 *
 * @author H.Sugimoto
 */
public class TruncateTest extends AbstractDbTest {

	/**
	 * 例外を発生させるためのダミーEntityクラス
	 */
	public static class Dummy {
		private int dummyId;

		public Dummy() {
		}

		public int getDummyId() {
			return dummyId;
		}

		public void setDummyId(final int dummyId) {
			this.dummyId = dummyId;
		}
	}

	/**
	 * TRUNCATEのテストケース。
	 */
	@Test
	public void testTruncate() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		var products = agent.query(Product.class).collect();
		assertThat(products.isEmpty(), is(false));

		products = agent.truncate(Product.class).query(Product.class).collect();
		assertThat(products.isEmpty(), is(true));
	}

	/**
	 * Entityクラスが不正な場合のテストケース。
	 */
	@Test
	public void testTruncateError() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		assertThrows(EntitySqlRuntimeException.class, () -> agent.truncate(Dummy.class));
	}

}