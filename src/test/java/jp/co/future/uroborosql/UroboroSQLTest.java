package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

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
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.connection.ConnectionContextBuilder;
import jp.co.future.uroborosql.connection.DataSourceConnectionContext;
import jp.co.future.uroborosql.connection.DataSourceConnectionSupplierImpl;
import jp.co.future.uroborosql.connection.DefaultConnectionSupplierImpl;
import jp.co.future.uroborosql.dialect.H2Dialect;
import jp.co.future.uroborosql.store.SqlResourceManagerImpl;
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.StringUtils;

public class UroboroSQLTest {
	private List<Map<String, Object>> getDataFromFile(final Path path) {
		List<Map<String, Object>> ans = new ArrayList<>();
		try {
			Files.readAllLines(path, StandardCharsets.UTF_8).forEach(line -> {
				Map<String, Object> row = new LinkedHashMap<>();
				var parts = line.split("\t");
				for (var part : parts) {
					var keyValue = part.split(":", 2);
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
		var dataList = getDataFromFile(path);
		dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
				.forEach(tbl -> agent.updateWith("truncate table " + tbl.toString()).count());
		dataList.stream().forEach(map -> agent.update(map.get("sql").toString()).paramMap(map).count());
	}

	@Test
	void builderWithConnection() throws Exception {
		var config = UroboroSQL
				.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName())).build();
		try (var agent = config.agent()) {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}
	}

	@Test
	void builderSetConnectionSupplier() throws Exception {
		var config = UroboroSQL
				.builder()
				.setConnectionSupplier(
						new DefaultConnectionSupplierImpl(
								DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName())))
				.build();
		try (var agent = config.agent()) {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}
	}

	@Test
	void builderSetUrl() throws Exception {
		var config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "").build();
		try (var agent = config.agent()) {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}
	}

	@Test
	void builderSetUrlWithSchema() throws Exception {
		var config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "", null).build();
		try (var agent = config.agent()) {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}
	}

	@Test
	void builderSetUrlMultiConnection() throws Exception {
		var config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "", null).build();

		var checkSql = "select table_name from information_schema.tables where table_name = 'PRODUCT'";
		try (var agent = config.agent(
				ConnectionContextBuilder.jdbc("jdbc:h2:mem:" + this.getClass().getSimpleName() + "Sub1", "", ""))) {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			assertThat(agent.queryWith(checkSql).collect().size(), is(1));
		}

		try (var agent = config.agent()) {
			assertThat(agent.queryWith(checkSql).collect().size(), is(0));
		}
	}

	@Test
	void builderSetSqlResourceManager() throws Exception {
		var config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "", null)
				.setSqlResourceManager(new SqlResourceManagerImpl(false)).build();
		try (var agent = config.agent()) {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
			agent.rollback();
		}

		assertEquals(true, config.getSqlResourceManager().existSql("ddl/create_tables"));
	}

	@Test
	void builderWithDataSource() throws Exception {
		var ds = new JdbcDataSource();
		ds.setURL("jdbc:h2:mem:" + this.getClass().getSimpleName());

		var config = UroboroSQL.builder(ds).build();
		try (var agent = config.agent()) {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
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
	void builderWithMultiDataSource() throws Exception {
		System.setProperty(Context.INITIAL_CONTEXT_FACTORY, "jp.co.future.uroborosql.connection.LocalContextFactory");
		System.setProperty(Context.URL_PKG_PREFIXES, "local");

		var ds1 = new JdbcDataSource();
		ds1.setURL("jdbc:h2:mem:" + this.getClass().getSimpleName() + "1");
		var ds2 = new JdbcDataSource();
		ds2.setURL("jdbc:h2:mem:" + this.getClass().getSimpleName() + "2");

		Context ic = new InitialContext();
		var dsName1 = DataSourceConnectionContext.DEFAULT_DATASOURCE_NAME;
		var dsName2 = "java:comp/env/jdbc/second_datasource";
		ic.createSubcontext("java:comp");
		ic.createSubcontext("java:comp/env");
		ic.createSubcontext("java:comp/env/jdbc");
		ic.bind(dsName1, ds1);
		ic.bind(dsName2, ds2);

		var config = UroboroSQL.builder()
				.setConnectionSupplier(new DataSourceConnectionSupplierImpl())
				.build();

		var checkSql = "select table_name from information_schema.tables where table_name = 'PRODUCT'";
		try (var agent = config.agent()) {
			agent.required(() -> {
				assertThat(agent.queryWith(checkSql).collect().size(), is(0));
				try {
					var sqls = new String(
							Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
							StandardCharsets.UTF_8).split(";");
					for (var sql : sqls) {
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

		try (var agent = config.agent(ConnectionContextBuilder
				.dataSource(dsName2)
				.autoCommit(true)
				.readOnly(true)
				.transactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED))) {
			assertThat(agent.queryWith(checkSql).collect().size(), is(0));
			try {
				var sqls = new String(
						Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
						StandardCharsets.UTF_8).split(";");
				for (var sql : sqls) {
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
	void builderWithSqlAgentFactory() throws Exception {
		var config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "")
				.setSqlAgentProvider(new SqlAgentProviderImpl().setDefaultMapKeyCaseFormat(CaseFormat.CAMEL_CASE))
				.build();
		try (var agent = config.agent()) {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}

			insert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

			agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
					.stream().forEach(m -> {
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
	void builderWithClock() throws Exception {
		var zoneId = ZoneId.of("Asia/Singapore");
		var clock = Clock.system(zoneId);
		var config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "", "")
				.setClock(clock)
				.build();

		assertThat(config.getClock().getZone(), is(zoneId));
	}

	@Test
	void builderConnectionSupplierNull() throws Exception {
		try {
			UroboroSQL.builder().build();
		} catch (IllegalStateException ex) {
			// OK
		} catch (Exception ex) {
			fail(ex.getMessage());
		}
	}

}