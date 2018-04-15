package jp.co.future.uroborosql;

import static org.junit.Assert.*;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.DefaultConnectionSupplierImpl;
import jp.co.future.uroborosql.dialect.H2Dialect;
import jp.co.future.uroborosql.store.SqlManagerImpl;
import jp.co.future.uroborosql.utils.CaseFormat;

import org.apache.commons.lang3.StringUtils;
import org.h2.jdbcx.JdbcDataSource;
import org.junit.Test;

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
		SqlConfig config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SqlAgentTest")).build();
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
						new DefaultConnectionSupplierImpl(DriverManager.getConnection("jdbc:h2:mem:SqlAgentTest")))
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
		SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:SqlAgentTest", "", "").build();
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
		SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:SqlAgentTest", "", "", null).build();
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
	public void builderSetSqlManager() throws Exception {
		SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:SqlAgentTest", "", "", null)
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
		ds.setURL("jdbc:h2:mem:SqlAgentTest");

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
	public void builderWithSqlAgentFactory() throws Exception {
		SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:SqlAgentTest", "", "")
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

			agent.query("example/select_product").paramList("product_id", 0, 1)
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