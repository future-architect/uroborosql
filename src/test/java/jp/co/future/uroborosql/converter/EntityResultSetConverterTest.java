package jp.co.future.uroborosql.converter;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.sql.DriverManager;
import java.util.Optional;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;

import org.apache.commons.lang3.StringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class EntityResultSetConverterTest {

	private SqlConfig config;

	private SqlAgent agent;

	public static class ColumnTypeTest2 {
		private String colClob;
		private String colNclob;
		private byte[] colBlob;
		private Object[] colArray;

		/**
		 * colClob を取得します。
		 *
		 * @return colClob
		 */
		public String getColClob() {
			return colClob;
		}

		/**
		 * colClob を設定します。
		 *
		 * @param colClob colClob
		 */
		public void setColClob(final String colClob) {
			this.colClob = colClob;
		}

		/**
		 * colNclob を取得します。
		 *
		 * @return colNclob
		 */
		public String getColNclob() {
			return colNclob;
		}

		/**
		 * colNclob を設定します。
		 *
		 * @param colNclob colNclob
		 */
		public void setColNclob(final String colNclob) {
			this.colNclob = colNclob;
		}

		/**
		 * colBlob を取得します。
		 *
		 * @return colBlob
		 */
		public byte[] getColBlob() {
			return colBlob;
		}

		/**
		 * colBlob を設定します。
		 *
		 * @param colBlob colBlob
		 */
		public void setColBlob(final byte[] colBlob) {
			this.colBlob = colBlob;
		}

		/**
		 * colArray を取得します。
		 *
		 * @return colArray
		 */
		public Object[] getColArray() {
			return colArray;
		}

		/**
		 * colArray を設定します。
		 *
		 * @param colArray colArray
		 */
		public void setColArray(final Object[] colArray) {
			this.colArray = colArray;
		}

	}

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:EntityResultSetConverterTest")).build();
		agent = config.agent();

		String sql = "create table if not exists COLUMN_TYPE_TEST2 (" +
				"	COL_CLOB			CLOB," +
				"	COL_NCLOB			NCLOB," +
				"	COL_BLOB			BLOB," +
				"	COL_ARRAY			ARRAY" +
				");";
		agent.updateWith(sql).count();

		agent.commit();
	}

	@After
	public void tearDown() throws Exception {
		agent.close();
	}

	@Test
	public void testCreateRecord() {
		String clob = StringUtils.repeat('1', 10000);
		String nclob = StringUtils.repeat('あ', 10000);
		byte[] blob = StringUtils.repeat('x', 20000).getBytes();
		Object[] arr = { 1, 2 };

		ColumnTypeTest2 entity = new ColumnTypeTest2();
		entity.setColClob(clob);
		entity.setColNclob(nclob);
		entity.setColBlob(blob);
		entity.setColArray(arr);

		agent.insert(entity);

		Optional<ColumnTypeTest2> optional = agent.queryWith("select * from COLUMN_TYPE_TEST2").findFirst(
				ColumnTypeTest2.class);
		assertThat(optional.isPresent(), is(true));
		ColumnTypeTest2 row = optional.get();

		assertThat(row.getColClob(), is(clob));
		assertThat(row.getColNclob(), is(nclob));
		assertThat(row.getColBlob(), is(blob));
		assertThat(row.getColArray(), is(arr));

	}

}
