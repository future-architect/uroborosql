package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.utils.StringUtils;

public class SqlAgentQueryWithIteratorTest {
	private SqlConfig config;

	private SqlAgent agent;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SqlAgentTest")).build();
		config.getSqlAgentProvider().setFetchSize(1000);
		agent = config.agent();
		var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (String sql : sqls) {
			if (StringUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}
		agent.commit();
	}

	@AfterEach
	public void tearDown() throws Exception {
		agent.close();
	}

	private List<Map<String, Object>> getDataFromFile(final Path path) {
		List<Map<String, Object>> ans = new ArrayList<>();
		try {
			Files.readAllLines(path, StandardCharsets.UTF_8).forEach(line -> {
				Map<String, Object> row = new LinkedHashMap<>();
				var parts = line.split("\t");
				for (String part : parts) {
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

	private void truncateTable(final Object... tables) {
		Arrays.asList(tables).stream().forEach(tbl -> {
			try {
				agent.updateWith("truncate table " + tbl.toString()).count();
			} catch (Exception ex) {
				ex.printStackTrace();
				assertThat("TABLE:" + tbl + " truncate is miss. ex:" + ex.getMessage(), false);
			}
		});
	}

	private void cleanInsert(final Path path) {
		var dataList = getDataFromFile(path);

		dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
				.forEach(this::truncateTable);

		dataList.forEach(map -> {
			try {
				agent.update(map.get("sql").toString()).paramMap(map).count();
			} catch (Exception ex) {
				ex.printStackTrace();
				assertThat("TABLE:" + map.get("TABLE") + " insert is miss. ex:" + ex.getMessage(), false);
			}
		});
	}

	@Test
	public void test() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// test
		Iterator<?> itr = agent.queryWith("select product_id from product")
				.stream()
				.iterator();

		var c = 0;
		while (itr.hasNext()) {
			itr.next();
			c++;
		}
		assertThat(c, is(2));
	}

	@Test
	public void testIssue() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// test
		Iterator<?> itr = agent.queryWith("select product_id from product")
				.stream()
				.map(r -> r)
				.iterator();

		var c = 0;
		while (itr.hasNext()) {
			itr.next();
			c++;
		}
		assertThat(c, is(2));
	}
}