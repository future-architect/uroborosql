package jp.co.future.uroborosql.converter;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.StringReader;
import java.sql.DriverManager;
import java.util.Map;
import java.util.Optional;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;

import org.apache.commons.lang3.StringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class MapResultSetConverterTest {

	private SqlConfig config;

	private SqlAgent agent;

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:MapResultSetConverterTest")).build();
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
		String sql = "insert into COLUMN_TYPE_TEST2 (" +
				"	COL_CLOB," +
				"	COL_NCLOB," +
				"	COL_BLOB," +
				"	COL_ARRAY" +
				") VALUES (" +
				"	/*clob*/, " +
				"	/*nclob*/, " +
				"	/*blob*/, " +
				"	/*arr*/" +
				")";

		String clob = StringUtils.repeat('1', 10000);
		String nclob = StringUtils.repeat('あ', 10000);
		byte[] blob = StringUtils.repeat('x', 20000).getBytes();
		int[] arr = { 1, 2 };
		agent.updateWith(sql).clobParam("clob", new StringReader(clob))
				.clobParam("nclob", new StringReader(nclob))
				.blobParam("blob", new ByteArrayInputStream(blob))
				.param("arr", arr)
				.count();

		Optional<Map<String, Object>> optional = agent.queryWith("select * from COLUMN_TYPE_TEST2").findFirst();
		assertThat(optional.isPresent(), is(true));
		Map<String, Object> row = optional.get();

		assertThat(row.get("COL_CLOB"), is(clob));
		assertThat(row.get("COL_NCLOB"), is(nclob));
		assertThat(row.get("COL_BLOB"), is(blob));
		assertThat(row.get("COL_ARRAY"), is(arr));

	}

}
