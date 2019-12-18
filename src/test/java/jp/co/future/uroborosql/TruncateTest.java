package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.nio.file.Paths;
import java.util.List;

import org.junit.Test;

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

		List<Product> products = agent.query(Product.class).collect();
		assertThat(products.isEmpty(), is(false));

		products = agent.truncate(Product.class).query(Product.class).collect();
		assertThat(products.isEmpty(), is(true));
	}

	/**
	 * Entityクラスが不正な場合のテストケース。
	 */
	@Test(expected = EntitySqlRuntimeException.class)
	public void testTruncateError() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		agent.truncate(Dummy.class);
	}

}