package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import org.junit.Test;

public class ArrayTypeTest extends AbstractDbTest {
	/**
	 * Array型のテスト
	 */
	@Test
	public void testArrayType() throws Exception {
		try {
			truncateTable("COLUMN_TYPE_ARRAY");
			String[] vals = { "aaa", "bbb" };
			agent.updateWith("insert into COLUMN_TYPE_ARRAY (COL_ARRAY) values (/*col_array*/)")
					.param("col_array", vals).count();
			assertThat(agent.queryWith("select COL_ARRAY from COLUMN_TYPE_ARRAY").first().get("COL_ARRAY"), is(vals));
		} catch (Exception ex) {
			fail(ex.getMessage());
		}
		try {
			truncateTable("COLUMN_TYPE_ARRAY");
			int[] vals = { 111, 222 };
			agent.updateWith("insert into COLUMN_TYPE_ARRAY (COL_ARRAY) values (/*col_array*/)")
					.param("col_array", vals).count();
			assertThat(agent.queryWith("select COL_ARRAY from COLUMN_TYPE_ARRAY").first().get("COL_ARRAY"), is(vals));
		} catch (Exception ex) {
			fail(ex.getMessage());
		}
		try {
			truncateTable("COLUMN_TYPE_ARRAY");
			Integer[] vals = { Integer.valueOf(111), Integer.valueOf(222) };
			agent.updateWith("insert into COLUMN_TYPE_ARRAY (COL_ARRAY) values (/*col_array*/)")
					.param("col_array", vals).count();
			assertThat(agent.queryWith("select COL_ARRAY from COLUMN_TYPE_ARRAY").first().get("COL_ARRAY"), is(vals));
		} catch (Exception ex) {
			fail(ex.getMessage());
		}
		try {
			truncateTable("COLUMN_TYPE_ARRAY");
			long[] vals = { 111L, 222L };
			agent.updateWith("insert into COLUMN_TYPE_ARRAY (COL_ARRAY) values (/*col_array*/)")
					.param("col_array", vals).count();
			assertThat(agent.queryWith("select COL_ARRAY from COLUMN_TYPE_ARRAY").first().get("COL_ARRAY"), is(vals));
		} catch (Exception ex) {
			fail(ex.getMessage());
		}
		try {
			truncateTable("COLUMN_TYPE_ARRAY");
			Long[] vals = { Long.valueOf(111L), Long.valueOf(222L) };
			agent.updateWith("insert into COLUMN_TYPE_ARRAY (COL_ARRAY) values (/*col_array*/)")
					.param("col_array", vals).count();
			assertThat(agent.queryWith("select COL_ARRAY from COLUMN_TYPE_ARRAY").first().get("COL_ARRAY"), is(vals));
		} catch (Exception ex) {
			fail(ex.getMessage());
		}
		try {
			truncateTable("COLUMN_TYPE_ARRAY");
			double[] vals = { 1111.11d, 2222.22d };
			agent.updateWith("insert into COLUMN_TYPE_ARRAY (COL_ARRAY) values (/*col_array*/)")
					.param("col_array", vals).count();
			assertThat(agent.queryWith("select COL_ARRAY from COLUMN_TYPE_ARRAY").first().get("COL_ARRAY"), is(vals));
		} catch (Exception ex) {
			fail(ex.getMessage());
		}
		try {
			truncateTable("COLUMN_TYPE_ARRAY");
			Double[] vals = { Double.valueOf(1111.11d), Double.valueOf(2222.22d) };
			agent.updateWith("insert into COLUMN_TYPE_ARRAY (COL_ARRAY) values (/*col_array*/)")
					.param("col_array", vals).count();
			assertThat(agent.queryWith("select COL_ARRAY from COLUMN_TYPE_ARRAY").first().get("COL_ARRAY"), is(vals));
		} catch (Exception ex) {
			fail(ex.getMessage());
		}
	}
}