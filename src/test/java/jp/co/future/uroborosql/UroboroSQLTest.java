package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.time.Clock;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.naming.Context;
import javax.naming.InitialContext;

import org.h2.jdbcx.JdbcDataSource;
import org.junit.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContextBuilder;
import jp.co.future.uroborosql.connection.DataSourceConnectionContext;
import jp.co.future.uroborosql.connection.DataSourceConnectionSupplierImpl;
import jp.co.future.uroborosql.connection.DefaultConnectionSupplierImpl;
import jp.co.future.uroborosql.dialect.H2Dialect;
import jp.co.future.uroborosql.store.SqlManagerImpl;
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.StringUtils;

public class UroboroSQLTest {
	private List<Map<String, Object>> getDataFromFile(final Path path) {
		List<Map<String, Object>> ans = new ArrayList<>();
		try {
			Files.readAllLines(path, StandardCharsets.UTF_8).forEach(line -> {
				Map<String, Object> row = new LinkedHashMap<>();
				String[] parts = line.split("\t");
				for (String part : parts) {
					String[] keyValue = part.split(":", 2);
					row.put(keyValue[0].toLowerCase(), StringUtils.isBlank(keyValue[1]) ? null : keyValue[1]);
				}
				ans.add(row);
			});
		} catch (IOException e) {
			e.printStackTrace();
		}
		return ans;
	}

	private void insert(final SqlAgent agent, final Path path) {
		List<Map<String, Object>> dataList = getDataFromFile(path);
		dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
				.forEach(tbl -> agent.updateWith("truncate table " + tbl.toString()).count());
		dataList.stream().forEach(map -> agent.update(map.get("sql").toString()).paramMap(map).count());
	}

	@Test
	public void builderWithConnection() throws Exception {
		SqlConfig config = UroboroSQL
				.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName())).build();
		try (SqlAgent agent = config.agent()) {
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}
	}

	@Test
	public void builderSetConnectionSupplier() throws Exception {
		SqlConfig config = UroboroSQL
				.builder()
				.setConnectionSupplier(
						new DefaultConnectionSupplierImpl(
								DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName())))
				.build();
		try (SqlAgent agent = config.agent()) {
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}
	}

	@Test
	public void builderSetUrl() throws Exception {
		SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "").build();
		try (SqlAgent agent = config.agent()) {
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}
	}

	@Test
	public void builderSetUrlWithSchema() throws Exception {
		SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "", null).build();
		try (SqlAgent agent = config.agent()) {
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}
	}

	@Test
	public void builderSetUrlMultiConnection() throws Exception {
		SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "", null).build();

		String checkSql = "select table_name from information_schema.tables where table_name = 'PRODUCT'";
		try (SqlAgent agent = config.agent(
				ConnectionContextBuilder.jdbc("jdbc:h2:mem:" + this.getClass().getSimpleName() + "Sub1", "", ""))) {
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			assertThat(agent.queryWith(checkSql).collect().size(), is(1));
		}

		try (SqlAgent agent = config.agent()) {
			assertThat(agent.queryWith(checkSql).collect().size(), is(0));
		}
	}

	@Test
	public void builderSetSqlManager() throws Exception {
		SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "", null)
				.setSqlManager(new SqlManagerImpl(false)).build();
		try (SqlAgent agent = config.agent()) {
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}

		assertEquals(false, config.getSqlManager().isCache());
	}

	@Test
	public void builderWithDataSource() throws Exception {
		JdbcDataSource ds = new JdbcDataSource();
		ds.setURL("jdbc:h2:mem:" + this.getClass().getSimpleName());

		SqlConfig config = UroboroSQL.builder(ds).build();
		try (SqlAgent agent = config.agent()) {
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}

		assertEquals(new H2Dialect().getDatabaseName(), config.getDialect().getDatabaseName());
	}

	@Test
	public void builderWithMultiDataSource() throws Exception {
		System.setProperty(Context.INITIAL_CONTEXT_FACTORY, "jp.co.future.uroborosql.connection.LocalContextFactory");
		System.setProperty(Context.URL_PKG_PREFIXES, "local");

		JdbcDataSource ds1 = new JdbcDataSource();
		ds1.setURL("jdbc:h2:mem:" + this.getClass().getSimpleName() + "1");
		JdbcDataSource ds2 = new JdbcDataSource();
		ds2.setURL("jdbc:h2:mem:" + this.getClass().getSimpleName() + "2");

		Context ic = new InitialContext();
		String dsName1 = DataSourceConnectionContext.DEFAULT_DATASOURCE_NAME;
		String dsName2 = "java:comp/env/jdbc/second_datasource";
		ic.createSubcontext("java:comp");
		ic.createSubcontext("java:comp/env");
		ic.createSubcontext("java:comp/env/jdbc");
		ic.bind(dsName1, ds1);
		ic.bind(dsName2, ds2);

		SqlConfig config = UroboroSQL.builder()
				.setConnectionSupplier(new DataSourceConnectionSupplierImpl())
				.build();

		String checkSql = "select table_name from information_schema.tables where table_name = 'PRODUCT'";
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				assertThat(agent.queryWith(checkSql).collect().size(), is(0));
				try {
					String[] sqls = new String(
							Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
							StandardCharsets.UTF_8).split(";");
					for (String sql : sqls) {
						if (StringUtils.isNotBlank(sql)) {
							agent.updateWith(sql.trim()).count();
						}
					}
				} catch (IOException e) {
					fail(e.getMessage());
				}
				insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
				assertThat(agent.query("example/select_product").collect().size(), is(2));
				assertThat(agent.queryWith(checkSql).collect().size(), is(1));
			});
		}

		try (SqlAgent agent = config.agent(ConnectionContextBuilder
				.dataSource(dsName2)
				.autoCommit(true)
				.readOnly(true)
				.transactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED))) {
			assertThat(agent.queryWith(checkSql).collect().size(), is(0));
			try {
				String[] sqls = new String(
						Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
						StandardCharsets.UTF_8).split(";");
				for (String sql : sqls) {
					if (StringUtils.isNotBlank(sql)) {
						agent.updateWith(sql.trim()).count();
					}
				}
			} catch (IOException e) {
				fail(e.getMessage());
			}
			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			assertThat(agent.query("example/select_product").collect().size(), is(2));
			assertThat(agent.queryWith(checkSql).collect().size(), is(1));
		}
	}

	@Test
	public void builderWithSqlAgentFactory() throws Exception {
		SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "")
				.setSqlAgentFactory(new SqlAgentFactoryImpl().setDefaultMapKeyCaseFormat(CaseFormat.CAMEL_CASE))
				.build();
		try (SqlAgent agent = config.agent()) {
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

			agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
					.stream().forEach((m) -> {
						assertTrue(m.containsKey("productId"));
						assertTrue(m.containsKey("productName"));
						assertTrue(m.containsKey("productKanaName"));
						assertTrue(m.containsKey("janCode"));
						assertTrue(m.containsKey("productDescription"));
						assertTrue(m.containsKey("insDatetime"));
						assertTrue(m.containsKey("updDatetime"));
						assertTrue(m.containsKey("versionNo"));
					});

			agent.rollback();
		}

		assertEquals(new H2Dialect().getDatabaseName(), config.getDialect().getDatabaseName());
	}

	@Test
	public void builderWithClock() throws Exception {
		ZoneId zoneId = ZoneId.of("Asia/Singapore");
		Clock clock = Clock.system(zoneId);
		SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "")
				.setClock(clock)
				.build();

		assertThat(config.getClock().getZone(), is(zoneId));
	}

	@Test
	public void builderConnectionSupplierNull() throws Exception {
		try {
			UroboroSQL.builder().build();
		} catch (IllegalStateException ex) {
			// OK
		} catch (Exception ex) {
			fail(ex.getMessage());
		}
	}

}