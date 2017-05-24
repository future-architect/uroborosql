package jp.co.future.uroborosql.store;

import junit.framework.TestCase;

public class SqlManagerTest extends TestCase {
	public void testGet() throws Exception {
		SqlManagerImpl sqlManagerImpl = new SqlManagerImpl();
		sqlManagerImpl.setSqlLoader(new SqlLoaderImpl());
		sqlManagerImpl.initialize();

		String sql = sqlManagerImpl.getSql("example/select_product");
		assertEquals(
				"SELECT /* _SQL_ID_ */" + System.lineSeparator() + "	*" + System.lineSeparator() + "FROM"
						+ System.lineSeparator() + "	PRODUCT" + System.lineSeparator() + "WHERE 1 = 1"
						+ System.lineSeparator() + "/*IF product_id != null */" + System.lineSeparator()
						+ "AND	PRODUCT_ID	IN	/*product_id*/(0, 2)" + System.lineSeparator() + "/*END*/"
						+ System.lineSeparator() + "ORDER BY PRODUCT_ID" + System.lineSeparator(), sql);
	}
}
