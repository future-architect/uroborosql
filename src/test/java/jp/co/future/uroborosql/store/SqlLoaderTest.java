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
				"SELECT /* _SQL_ID_ */" + System.lineSeparator() + "	*" + System.lineSeparator() + "FROM"
						+ System.lineSeparator() + "	PRODUCT" + System.lineSeparator() + "WHERE 1 = 1"
						+ System.lineSeparator() + "/*IF product_id != null */" + System.lineSeparator()
						+ "AND	PRODUCT_ID	IN	/*product_id*/(0, 2)" + System.lineSeparator() + "/*END*/"
						+ System.lineSeparator() + "ORDER BY PRODUCT_ID" + System.lineSeparator(), sql);
	}

	public void testLoadFile() throws Exception {
		SqlLoader sqlLoader = new SqlLoaderImpl();
		String sql = sqlLoader.load("example/select_product");
		assertEquals(
				"SELECT /* _SQL_ID_ */" + System.lineSeparator() + "	*" + System.lineSeparator() + "FROM"
						+ System.lineSeparator() + "	PRODUCT" + System.lineSeparator() + "WHERE 1 = 1"
						+ System.lineSeparator() + "/*IF product_id != null */" + System.lineSeparator()
						+ "AND	PRODUCT_ID	IN	/*product_id*/(0, 2)" + System.lineSeparator() + "/*END*/"
						+ System.lineSeparator() + "ORDER BY PRODUCT_ID" + System.lineSeparator(), sql);
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
