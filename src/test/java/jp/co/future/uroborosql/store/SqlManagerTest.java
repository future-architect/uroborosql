package jp.co.future.uroborosql.store;

import junit.framework.TestCase;

public class SqlManagerTest extends TestCase {
	public void testGet() throws Exception {
		SqlManagerImpl sqlManagerImpl = new SqlManagerImpl();
		sqlManagerImpl.setSqlLoader(new SqlLoaderImpl());
		sqlManagerImpl.initialize();

		String sql = sqlManagerImpl.getSql("example/select_product");
		assertEquals(
				"select /* _SQL_ID_ */ * from product where product_id in /*product_id*/(0, 2)"
						+ System.lineSeparator(), sql);
	}
}
