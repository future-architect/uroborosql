package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.sql.JDBCType;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.parameter.ReaderParameter;
import jp.co.future.uroborosql.parameter.StreamParameter;

public class AbstractSqlFluentTest {
	private static SqlConfig config = null;

	@BeforeAll
	public static void setUpClass() {
		config = UroboroSQL.builder("jdbc:h2:mem:ExecutionContextImplTest", "sa", "").build();
	}

	@BeforeEach
	public void setUp() throws Exception {
	}

	@AfterEach
	public void tearDown() throws Exception {
	}

	@Test
	public void testHasParam() throws Exception {
		try (var agent = config.agent()) {
			SqlQuery query = null;
			query = agent.query("select * from dummy");
			query.param("key1", "value1");
			assertThat(query.hasParam("key1"), is(true));
			assertThat(query.hasParam("key2"), is(false));
		}
	}

	@Test
	public void testparamList() throws Exception {
		try (var agent = config.agent()) {
			SqlQuery query = null;
			query = agent.query("select * from dummy");
			query.param("key1", List.of("value1"));
			assertThat(query.context().getParam("key1").getValue(), is(List.of("value1")));

			query = agent.query("select * from dummy");
			query.param("key1", List.of("value1", "value2"));
			assertThat(query.context().getParam("key1").getValue(), is(List.of("value1", "value2")));

			query = agent.query("select * from dummy");
			query.param("key1", () -> List.of("value1", "value2"));
			assertThat(query.context().getParam("key1").getValue(), is(List.of("value1", "value2")));
		}

	}

	@Test
	public void testParamList() throws Exception {
		try (var agent = config.agent()) {
			SqlQuery query = null;
			query = agent.query("select * from dummy");
			query.param("key1", List.of("value1"));
			assertThat(query.context().getParam("key1").getValue(), is(List.of("value1")));

			query = agent.query("select * from dummy");
			query.param("key1", List.of("value1", "value2"));
			assertThat(query.context().getParam("key1").getValue(), is(List.of("value1", "value2")));

			Set<String> values = new HashSet<>();
			values.add("value1");
			values.add("value2");

			query = agent.query("select * from dummy");
			query.param("key1", values);
			assertThat(query.context().getParam("key1").getValue(), is(values));

			query = agent.query("select * from dummy");
			query.param("key1", () -> values);
			assertThat(query.context().getParam("key1").getValue(), is(values));
		}
	}

	@Test
	public void testIfAbsent() throws Exception {
		try (var agent = config.agent()) {
			SqlQuery query = null;
			query = agent.query("select * from dummy");
			query.paramIfAbsent("key1", "value1");
			assertThat(query.context().getParam("key1").getValue(), is("value1"));
			query.paramIfAbsent("key1", "value2");
			assertThat(query.context().getParam("key1").getValue(), is("value1"));

			query = agent.query("select * from dummy");
			query.paramIfAbsent("key1", "value1", JDBCType.VARCHAR);
			assertThat(query.context().getParam("key1").getValue(), is("value1"));
			query.paramIfAbsent("key1", "value2", JDBCType.VARCHAR);
			assertThat(query.context().getParam("key1").getValue(), is("value1"));

			query = agent.query("select * from dummy");
			query.paramIfAbsent("key1", "value1", JDBCType.VARCHAR.getVendorTypeNumber());
			assertThat(query.context().getParam("key1").getValue(), is("value1"));
			query.paramIfAbsent("key1", "value2", JDBCType.VARCHAR.getVendorTypeNumber());
			assertThat(query.context().getParam("key1").getValue(), is("value1"));

			query = agent.query("select * from dummy");
			InputStream is1 = new ByteArrayInputStream("value1".getBytes());
			query.blobParamIfAbsent("key1", is1, "value1".length());
			var stream1 = (StreamParameter) query.context().getParam("key1");
			assertThat(query.context().getParam("key1").getValue(), is("[BLOB]"));
			InputStream is2 = new ByteArrayInputStream("value2".getBytes());
			query.blobParamIfAbsent("key1", is2, "value2".length());
			assertThat(query.context().getParam("key1"), is(stream1));

			query = agent.query("select * from dummy");
			InputStream is11 = new ByteArrayInputStream("value1".getBytes());
			query.blobParamIfAbsent("key1", is11);
			var stream11 = (StreamParameter) query.context().getParam("key1");
			assertThat(query.context().getParam("key1").getValue(), is("[BLOB]"));
			InputStream is22 = new ByteArrayInputStream("value2".getBytes());
			query.blobParamIfAbsent("key1", is22);
			assertThat(query.context().getParam("key1"), is(stream11));

			query = agent.query("select * from dummy");
			Reader r1 = new StringReader("value1");
			query.clobParamIfAbsent("key1", r1);
			var reader1 = (ReaderParameter) query.context().getParam("key1");
			assertThat(query.context().getParam("key1").getValue(), is("[CLOB]"));
			Reader r2 = new StringReader("value2");
			query.clobParamIfAbsent("key1", r2);
			assertThat(query.context().getParam("key1"), is(reader1));

			query = agent.query("select * from dummy");
			Reader r11 = new StringReader("value1");
			query.clobParamIfAbsent("key1", r11, "value1".length());
			var reader11 = (ReaderParameter) query.context().getParam("key1");
			assertThat(query.context().getParam("key1").getValue(), is("[CLOB]"));
			Reader r22 = new StringReader("value2");
			query.clobParamIfAbsent("key1", r22, "value1".length());
			assertThat(query.context().getParam("key1"), is(reader11));
		}
	}

	@Test
	public void testSqlId() throws Exception {
		try (var agent = config.agent()) {
			agent.update("ddl/create_tables").count();
			agent.update("setup/insert_product").count();

			final var sqlId = "SQL_ID_TEST";

			var query = agent.query("example/select_product").param("product_id", 1).sqlId(sqlId);
			assertThat(query.collect().size(), is(1));
			assertThat(query.context().getExecutableSql(), containsString(sqlId));
		}
	}

	@Test
	public void testParamWithSupplier() throws Exception {
		try (var agent = config.agent()) {
			SqlQuery query = null;
			query = agent.query("select * from dummy");
			query.param("key1", () -> "value1");
			assertThat(query.context().getParam("key1").getValue(), is("value1"));

			var flag = false;
			query.param("key2", () -> flag ? "true" : "false");
			assertThat(query.context().getParam("key2").getValue(), is("false"));

			query.param("key3", () -> List.of("a", "b", "c"));
			assertThat(query.context().getParam("key3").getValue(), is(List.of("a", "b", "c")));

			query.param("key4", null);
			assertThat(query.context().getParam("key4").getValue(), nullValue());

			query.param("key5", List.of());
			assertThat(query.context().getParam("key5").getValue(), is(List.of()));
		}
	}
}
