package jp.co.future.uroborosql.store;

import jp.co.future.uroborosql.store.SqlLoaderImpl;
import jp.co.future.uroborosql.store.SqlManagerImpl;
import junit.framework.TestCase;

public class SqlManagerTest extends TestCase {
	public void testGet() throws Exception {
		SqlManagerImpl sqlManagerImpl = new SqlManagerImpl();
		sqlManagerImpl.setSqlLoader(new SqlLoaderImpl());
		sqlManagerImpl.initialize();

		String sql = sqlManagerImpl.getSql("example.example");
		assertEquals("select * from product where product_id = :product_id" + System.lineSeparator(), sql);
	}
}
