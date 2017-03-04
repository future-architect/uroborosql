package jp.co.future.uroborosql.store;

import java.util.Map;

import jp.co.future.uroborosql.store.SqlLoader;
import jp.co.future.uroborosql.store.SqlLoaderImpl;
import junit.framework.TestCase;

public class SqlLoaderTest extends TestCase {

	public void testLoad() {
		SqlLoader sqlLoader = new SqlLoaderImpl();
		Map<String, String> sqls = sqlLoader.load();
		assertNotNull(sqls);

		String sql = sqls.get("example/example");
		assertEquals("select * from product where product_id = :product_id" + System.lineSeparator(), sql);
	}

	public void testLoadFile() throws Exception {
		SqlLoader sqlLoader = new SqlLoaderImpl();
		String sql = sqlLoader.load("example.example");
		assertEquals("select * from product where product_id = :product_id" + System.lineSeparator(), sql);
	}

	public void testLoadFileNull() throws Exception {
		SqlLoader sqlLoader = new SqlLoaderImpl();
		try {
			sqlLoader.load(null);
			fail();
		} catch (IllegalArgumentException ex) {
			// OK
		}
	}
}
