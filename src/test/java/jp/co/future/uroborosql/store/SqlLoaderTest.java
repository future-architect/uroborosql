package jp.co.future.uroborosql.store;

import java.util.Map;

import junit.framework.TestCase;

public class SqlLoaderTest extends TestCase {

	public void testLoad() {
		SqlLoader sqlLoader = new SqlLoaderImpl();
		Map<String, String> sqls = sqlLoader.load();
		assertNotNull(sqls);

		String sql = sqls.get("example/select_product");
		assertEquals(
				"select /* _SQL_ID_ */ * from product where product_id in /*product_id*/(0, 2)"
						+ System.lineSeparator(), sql);
	}

	public void testLoadFile() throws Exception {
		SqlLoader sqlLoader = new SqlLoaderImpl();
		String sql = sqlLoader.load("example/select_product");
		assertEquals(
				"select /* _SQL_ID_ */ * from product where product_id in /*product_id*/(0, 2)"
						+ System.lineSeparator(), sql);
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
