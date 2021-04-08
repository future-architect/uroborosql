package jp.co.future.uroborosql.event;

import static org.hamcrest.MatcherAssert.*;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.utils.StringUtils;

class BaseEventSubscriberTest {

	protected void createTables(final SqlAgent agent) throws IOException {
		var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (String sql : sqls) {
			if (StringUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}
	}

	protected void cleanInsert(final SqlAgent agent, final Path path) {
		var dataList = getDataFromFile(path);

		try {
			dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
					.forEach(tbl -> truncateTable(agent, tbl));

			dataList.stream().forEach(map -> {
				try {
					agent.update(map.get("sql").toString()).paramMap(map).count();
				} catch (Exception ex) {
					ex.printStackTrace();
					assertThat("TABLE:" + map.get("TABLE") + " insert is miss. ex:" + ex.getMessage(), false);
				}
			});
		} catch (Exception ex) {
			ex.printStackTrace();
			assertThat(ex.getMessage(), false);
		}
	}

	protected void truncateTable(final SqlAgent agent, final Object... tables) {
		try {
			Arrays.asList(tables).stream().forEach(tbl -> {
				try {
					agent.updateWith("truncate table " + tbl.toString()).count();
				} catch (Exception ex) {
					ex.printStackTrace();
					assertThat("TABLE:" + tbl + " truncate is miss. ex:" + ex.getMessage(), false);
				}
			});
		} catch (Exception ex) {
			ex.printStackTrace();
			assertThat(ex.getMessage(), false);
		}
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
}
